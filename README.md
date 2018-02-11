# nirc

Pronounced "nerk", `nirc` is a simple webservice for receiving push
notifications on mobile for instant messaging services. It was designed with IRC
in mind, and can be integrated with ZNC via the plugin [`znc-push`][].

## Rationale

I personally found `znc-push` to not work very well with Pushbullet. I wanted to
replicate the experience of modern mainstream instant messaging applications, in
which multiple chat events from the same context are coalesced into one single
notification. Achieving this by extending znc-push seemed complicated, since it
would require a more complicated interaction with the Pushbullet API than I felt
like coding up in a ZNC module in C++.

Instead, I would rather make use of [my pushbullet libraries][pbhs] and write a
simple webservice in Haskell using Servant. Then, I can hook this up to ZNC by
using `znc-push`'s `url` mode, in which it sends HTTP requests to a given URL.

## Setup

It should suffice to run `stack install` to compile the dependencies, the
executable, and copy it to `$HOME/.local/bin`.

## Use

To run `nirc`, the environment variable `PUSHBULLET_KEY` must be set to your
Pushbullet API key.

Only one endpoint is exposed:

  * `POST /activity/:network/:channel?sender=:sender&message=:message`

To configure this with `znc-push`:

```
/znc *push service url
/znc *push message_uri http://example.com/activity/{network}/{context}?sender={nick}&message={message}
```

Configure other `znc-push` settings as you normally would.

[znc-push]: https://github.com/jreese/znc-push
[pbhs]: https://github.com/tsani/pushbullet-hs
