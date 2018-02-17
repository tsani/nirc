module Main where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.IO.Class ( liftIO )
import Data.Coerce ( coerce )
import Data.Text ( Text )
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty ((<|),  NonEmpty )
import Data.Monoid ( (<>) )
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Map as M
import GHC.Generics
import Network.HTTP.Client.TLS ( newTlsManager )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Pushbullet.Api
import Network.Pushbullet.Client
import Network.Pushbullet.Types
import Servant.API
import Servant.Server
import Servant.Client
import System.Environment ( getEnv )
import System.Exit ( exitFailure, exitSuccess )
import Web.FormUrlEncoded

----- COMMON -----
------------------

recentsCount :: Int
recentsCount = 4

newtype Network
  = Network Text
  deriving stock (Eq, Ord)
  deriving newtype FromHttpApiData


newtype Channel
  = Channel Text
  deriving stock (Eq, Ord)
  deriving newtype FromHttpApiData

newtype Sender
  = Sender Text
  deriving newtype FromHttpApiData

newtype Message
  = Message Text
  deriving newtype FromHttpApiData

class Boolean a where
  top :: a
  (&&&) :: a -> a -> a
  (|||) :: a -> a -> a
  bot :: a

anyBoolean :: Boolean a => [a] -> a
anyBoolean [] = bot
anyBoolean (x:xs) = x ||| anyBoolean xs

allBoolean :: Boolean a => [a] -> a
allBoolean [] = top
allBoolean (x:xs) = x &&& allBoolean xs

-- | A highlighter decides whether an activity counts as a highlight.
newtype Highlighter
  = Highlighter { highlight :: Activity -> Bool }

-- | A highlighter that checks whether the given string appears as a
-- substring of the message body.
hlBodySubstring :: Text -> Highlighter
hlBodySubstring t = Highlighter $ \Activity{..} ->
  let (Message msg) = actMessage in
    t `T.isInfixOf` msg

-- | Finite boolean algebras.
instance Boolean Highlighter where
  top = Highlighter (const True)
  bot = Highlighter (const False)
  Highlighter f &&& Highlighter g = Highlighter $ \x -> f x && g x
  Highlighter f ||| Highlighter g = Highlighter $ \x -> f x || g x

-- | A map that associates each context with the last notification
-- sent to the user regarding that context.
type NotifyMap = M.Map Ctx PushNotification

-- | A push notification is the ID of the push we sent and the
-- information contained inside it.
type PushNotification = (PushId, NotifyData)

-- | An IRC context in a place in which chatting can take place.
-- Such a place is uniquely identified by the network and channel in
-- which that chats took place.
type Ctx = (Network, Channel)

-- | The data contained a push notification sent to a user.
data NotifyData
  = NotifyData
    { notActivity :: NonEmpty Activity
    }

type NotifyDataFormatter = NotifyData -> Push 'New

formatHighlightActivity :: NotifyDataFormatter
formatHighlightActivity NotifyData{..} =
  simpleNewPush ToAll NotePush
    { pushTitle = Just "Highlights"
    , pushBody = formatBody notActivity
    }
  where
    formatBody = foldr f T.empty where
      f Activity{..} t = n <> chan <> s <> ": " <> msg <> "\n" <> t where
        n = net <> ": "
        -- only show the channel when it's not a private message
        chan = if c == s then "" else c <> ": "

        (Sender s) = actSender
        (Channel c) = actChannel
        (Message msg) = actMessage
        (Network net) = actNetwork

formatRegularActivity :: Ctx -> NotifyDataFormatter
formatRegularActivity ctx NotifyData{..} =
  simpleNewPush ToAll NotePush
    { pushTitle = Just (formatContext ctx)
    , pushBody = formatActivity notActivity
    }
  where
    formatContext (Network n, Channel c) = c <> "@" <> n
    -- fold over the activity entries; they are stored most-recent first
    formatActivity :: NonEmpty Activity -> Text
    formatActivity as = formatMessages recents <> formatOthers olds where
      (recents, olds) = splitAt recentsCount (N.toList as)
    formatOthers l
      | null l = ""
      | otherwise = "(and " <> T.pack (show (length l)) <> " other messages)"
    formatMessages = foldr f T.empty where
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
  deriving (FromForm, Generic)


-- | Computes the context in which a given IRC activity occurred.
actContext :: Activity -> Ctx
actContext Activity{..} = (actNetwork, actChannel)

----- SERVER -----
------------------

-- | The API of the webservice.
type API
  = "activity"
  :> ReqBody '[FormUrlEncoded] Activity
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
server ServerEnv{sendEvent} = activity where
  activity :: Activity -> Handler ()
  activity = liftIO . sendEvent . ActivityDetected

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
    , hlPush :: Maybe PushNotification
    , hl :: Highlighter
    }

-- | Prepends the given 'Activity' to the given 'NotifyData' if the
-- associated 'PushId' has not yet been dismissed. In doing so, the
-- exiting push is deleted.
adjustPush :: NotifyData -> Activity -> PushId -> HttpEnv -> IO NotifyData
adjustPush NotifyData{..} a p e@HttpEnv{..} = do
  let newNot = NotifyData { notActivity = pure a }
  pushDismissed <$> runClient e (getPush auth p) >>= \case
    True -> pure newNot
    False -> do
      runClient e (deletePush auth p)
      pure NotifyData { notActivity = a <| notActivity }

runClient :: HttpEnv -> ClientM a -> IO a
runClient HttpEnv{..} = retryingDelay timeoutDelay . flip runClientM clientEnv

-- | An infinite loop that services requests sent through the 'Chan'
-- inside the 'HttpEnv'.
http :: HttpEnv -> IO a
http e@HttpEnv{..} = do
  f <- recvEvent >>= \case
    ActivityDetected a -> do
      let ctx = actContext a
      let newNot = NotifyData { notActivity = pure a }
      let pm i d = M.insert ctx (i, d) pushMap
      -- decide whether the activity is a highlight, or whether it
      -- appears in the pushmap
      (p, f) <- case (highlight hl a, M.lookup ctx pushMap) of
        (True, _) -> do
          n' <- case hlPush of
            Just (hli, hlnd) -> adjustPush hlnd a hli e
            Nothing -> pure newNot
          pure (formatHighlightActivity n', \i s -> s { hlPush = Just (i, n') })
        (_, Just (i, nd)) -> do
          n' <- adjustPush nd a i e
          pure (formatRegularActivity ctx n', \j s -> s { pushMap = pm j n'})
        (_, Nothing) -> do
          let n' = newNot
          pure (formatRegularActivity ctx n', \i s -> s { pushMap = pm i n'})

      -- create the new push
      p' <- runClient e (createPush auth p)

      -- save the new notification data and ID of the new push into
      -- the map
      pure (f (pushId p'))

  -- recurse after applying the changes to the environment
  http (f e)

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
  hlwords <- T.words . T.pack <$> getEnv "NIRC_HL_WORDS"

  let p = 8088
  manager <- newTlsManager
  c <- newChan
  let sendEvent = writeChan c

  let clientEnv = ClientEnv manager baseurl
  let pushMap = M.empty
  let recvEvent = readChan c
  let hl = allBoolean $ hlBodySubstring <$> hlwords
  let hlPush = Nothing
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
