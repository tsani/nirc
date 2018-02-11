import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.IO.Class ( liftIO )
import Data.Text ( Text )
import Data.List.NonEmpty ((<|),  NonEmpty )
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Proxy
import qualified Data.Map as M
import Network.HTTP.Client.TLS ( newTlsManager )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types
import Web.HttpApiData
import Servant.API
import Servant.Server
import Servant.Client
import System.Environment ( getEnv )
import System.Exit ( exitFailure, exitSuccess )

----- COMMON -----
------------------

newtype Network
  = Network Text
  deriving (FromHttpApiData, Eq, Ord)

newtype Channel
  = Channel Text
  deriving (FromHttpApiData, Eq, Ord)

newtype Sender
  = Sender Text
  deriving FromHttpApiData

newtype Message
  = Message Text
  deriving FromHttpApiData

-- | A map that associates each context with the last notification
-- sent to the user regarding that context.
type NotifyMap = M.Map Ctx (PushId, NotifyData)

-- | An IRC context in a place in which chatting can take place.
-- Such a place is uniquely identified by the network and channel in
-- which that chats took place.
type Ctx = (Network, Channel)

-- | The data contained a push notification sent to a user.
data NotifyData
  = NotifyData
    { notActivity :: NonEmpty Activity
    }

formatNotifyData :: Ctx -> NotifyData -> Push 'New
formatNotifyData ctx NotifyData{..} =
  simpleNewPush ToAll NotePush
    { pushTitle = Just (formatContext ctx)
    , pushBody = formatActivity notActivity
    }
  where
    formatContext (Network n, Channel c) = c <> "@" <> n
    -- fold over the activity entries; they are stored most-recent first
    formatActivity = foldr f T.empty where
      f Activity{..} t = s <> ": " <> msg <> "\n" <> t where
        (Sender s) = actSender
        (Message msg) = actMessage

-- | Events that can occur on IRC.
data Event
  = ActivityDetected Activity

-- | IRC activity.
data Activity
  = Activity
    { actSender :: Sender
    , actChannel :: Channel
    , actNetwork :: Network
    , actMessage :: Message
    }

-- | Computes the context in which a given IRC activity occurred.
actContext :: Activity -> Ctx
actContext Activity{..} = (actNetwork, actChannel)

----- SERVER -----
------------------

-- | The API of the webservice.
type API
  = "jake"
  :> Capture "network" Network
  :> Capture "channel" Channel
  :> QueryParam "sender" Sender
  :> QueryParam "message" Message
  :> Post '[JSON] ()

api :: Proxy API
api = Proxy

-- | The environment for the server thread.
data ServerEnv
  = ServerEnv
    { sendEvent :: Event -> IO ()
    }

-- | The webservice.
server :: ServerEnv -> Server API
server ServerEnv{..} = activity where
  activity :: Network -> Channel -> Maybe Sender -> Maybe Message -> Handler ()
  activity n c (Just s) (Just m) = liftIO $ do
    putStrLn "Got message!"
    sendEvent $ ActivityDetected Activity
      { actNetwork = n
      , actChannel = c
      , actSender = s
      , actMessage = m
      }
  activity _ _ _ _ = liftIO $ putStrLn "uh-oh"

----- CLIENT -----
-- The client thread's entry point is the 'http' function. This thread
-- pumps messages received on a Chan and responds to each event
-- appropriately.
------------------

-- | The environment for the http client thread.
data HttpEnv
  = HttpEnv
    { recvEvent :: IO Event
    , auth :: Auth
    , pushMap :: NotifyMap
    , clientEnv :: ClientEnv
    }

-- | An infinite loop that services requests sent through the 'Chan'
-- inside the 'HttpEnv'.
http :: HttpEnv -> IO a
http e@HttpEnv{..} = do
  pushMap' <- recvEvent >>= \case
    ActivityDetected a -> do
      putStrLn "detected activity"
      -- delete the existing push, if any, and construct/update the
      -- NotifyData
      let ctx = actContext a
      let newNot = NotifyData { notActivity = pure a }
      d <- case M.lookup ctx pushMap of
        Just (p, NotifyData{..}) -> do
          putStrLn "got existing push"
          -- first, check whether the existing push has been dismissed
          -- by the user
          pushDismissed <$> runClient (getPush auth p) >>= \case
            -- if it has, then we cook up a new notification
            True -> pure newNot
            -- if it hasn't, then we delete it, and update the
            -- existing notification
            False -> do
              runClient (deletePush auth p)
              pure NotifyData { notActivity = a <| notActivity }
        Nothing -> do
          putStrLn "no existing push"
          pure newNot

      -- create the new push
      let p = formatNotifyData ctx d
      p' <- runClient (createPush auth p)

      -- save the new notification data and ID of the new push into
      -- the map
      pure $ M.insert ctx (pushId p', d) pushMap

  -- recurse with m', which contains any updated state
  http e {pushMap = pushMap'}
  where
    runClient = retryingDelay timeoutDelay . flip runClientM clientEnv

-- | The base URL to which we send Pushbullet API calls.
baseurl :: BaseUrl
baseurl = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "api.pushbullet.com"
  , baseUrlPort = 443
  , baseUrlPath = ""
  }

----- MAIN -----
----------------

main :: IO ()
main = do
  auth <- pushbulletAuth . PushbulletKey . T.pack <$> getEnv "PUSHBULLET_KEY"
  let p = 8088
  manager <- newTlsManager
  c <- newChan
  let sendEvent = writeChan c

  let clientEnv = ClientEnv manager baseurl
  let pushMap = M.empty
  let recvEvent = readChan c
  let httpEnv = HttpEnv {..}
  httpThread <- async $ do
    putStrLn $ "HTTP client thread started."
    http httpEnv

  let serverEnv = ServerEnv {..}
  serverThread <- async $ do
    putStrLn $ "listening on port: " ++ show p
    run p (logStdoutDev $ serve api (server serverEnv))

  waitAnyCatch [httpThread, serverThread] >>= \case
    (_, e) -> case e of
      Left err -> print err *> exitFailure
      Right _ -> exitSuccess

----- MISC -----
----------------

timeoutDelay :: Int
timeoutDelay = 5000000

-- | Retry an IO action that can have an expected failure, delaying a
-- fixed amount between retries.
retryingDelay :: Show e => Int -> IO (Either e a) -> IO a
retryingDelay n m = either loop pure =<< m where
  loop e = do
    putStrLn $ "retrying... " ++ show e
    threadDelay n
    retryingDelay n m
