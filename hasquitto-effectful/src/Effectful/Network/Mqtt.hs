{- | A thin [@effectful@](https://hackage.haskell.org/package/effectful) wrapper over
"Network.Mqtt.Client.AutoReconnect": a dynamic, IO-backed 'Mqtt' effect for talking to /the/
auto-reconnecting MQTT v5 client that the interpreter holds. Unlike the underlying module, the
'AutoClient' is supplied once — to 'runMqtt' \/ 'runMqttWith' — instead of threaded through
every call, so effect code reads as plain @publish t b o@, @subscribe1 f q@, @recvMessage@, …
Intended for qualified import:

@
import Effectful.Network.Mqtt qualified as Mqtt
@

== Usage

Acquire a client with 'connect' \/ 'withClient' (plain 'IOE' helpers), then scope the effect
with 'runMqttWith'; or do both at once with 'runMqtt':

@
runEff '$' 'runMqtt' opts cfg '$' do
  _ <- 'subscribe1' myFilter QoS1
  'publish_' myTopic \"hello\"
  forever ('recvMessage' >>= handle)
@

'runMqtt' connects and runs but does __not__ disconnect; for bracketed teardown use
@'withClient' opts cfg \\ac s -> 'runMqttWith' ac s …@. The held handle and the initial
handshake 'Session' are available inside the effect via 'getClient' and 'getSession' (e.g. to
use the pure 'recvMessageSTM' or 'underlying').

The configuration ('AutoReconnectConfig', 'BackoffConfig'), the protocol vocabulary, the
received-message type ("Network.Mqtt.Message"), and the exception hierarchy
("Network.Mqtt.Exception") are re-exported here, so a caller imports only this module. The
blocking behaviour, delivery semantics, and reconnect caveats are exactly those of
"Network.Mqtt.Client.AutoReconnect" — read its Haddock.

The 'Mqtt' constructors are exported so you can write your own interpreter (for example a mock
for tests); ordinary code only needs the smart constructors and 'runMqtt' \/ 'runMqttWith'.
-}
module Effectful.Network.Mqtt (
  -- * The effect
  Mqtt (..),
  runMqtt,
  runMqttWith,

  -- * Acquiring a client (plain 'IOE' helpers)
  connect,
  withClient,

  -- * The held client & session
  getClient,
  getSession,

  -- * Status
  status,
  isConnected,

  -- * Messaging
  publish,
  publish_,
  subscribe,
  subscribe1,
  unsubscribe,
  ping,
  subscriptions,

  -- * Receiving
  recvMessage,
  tryRecvMessage,
  recvMessageSTM,

  -- * Configuration & handle (re-exported from "Network.Mqtt.Client.AutoReconnect")
  AutoClient,
  underlying,
  Status (..),
  BackoffConfig (..),
  defaultBackoffConfig,
  AutoReconnectConfig (..),
  defaultAutoReconnectConfig,

  -- * Connecting (re-exported from "Network.Mqtt.Client")
  ConnectOptions (..),
  defaultConnectOptions,
  PublishOptions (..),
  defaultPublishOptions,
  PublishResult (..),
  OverflowPolicy (..),
  TopicAliasMode (..),
  Authenticator (..),
  AuthChallenge (..),
  AuthResponse (..),
  Client,
  Session (..),
  Conn,

  -- * Re-exported protocol vocabulary
  module Network.Mqtt.Types.QoS,
  module Network.Mqtt.Types.Topic,
  module Network.Mqtt.Types.ReasonCode,
  module Network.Mqtt.Types.Property,
  module Network.Mqtt.Types.Will,
  Subscription (..),
  RetainHandling (..),
  module Network.Mqtt.Message,
  module Network.Mqtt.Exception,
  module Network.Mqtt.Connection.TCP,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, liftIO, withSeqEffToIO, type (:>))
import Effectful.Dispatch.Dynamic (interpret_, send)
import Network.Mqtt.Client (
  AuthChallenge (..),
  AuthResponse (..),
  Authenticator (..),
  Client,
  ConnectOptions (..),
  OverflowPolicy (..),
  PublishOptions (..),
  PublishResult (..),
  Session (..),
  TopicAliasMode (..),
  defaultConnectOptions,
  defaultPublishOptions,
 )
import Network.Mqtt.Client.AutoReconnect (
  AutoClient,
  AutoReconnectConfig (..),
  BackoffConfig (..),
  Status (..),
  defaultAutoReconnectConfig,
  defaultBackoffConfig,
  recvMessageSTM,
  underlying,
 )
import Network.Mqtt.Client.AutoReconnect qualified as Auto
import Network.Mqtt.Connection (Conn)
import Network.Mqtt.Connection.TCP
import Network.Mqtt.Exception
import Network.Mqtt.Message
import Network.Mqtt.Types.Packet (RetainHandling (..), Subscription (..))
import Network.Mqtt.Types.Property
import Network.Mqtt.Types.QoS
import Network.Mqtt.Types.ReasonCode
import Network.Mqtt.Types.Topic
import Network.Mqtt.Types.Will

-- The effect ----------------------------------------------------------------

{- | The MQTT capability: operations on /the/ auto-reconnecting MQTT v5 client that the
interpreter ('runMqtt' \/ 'runMqttWith') holds. Each operation corresponds to one of
"Network.Mqtt.Client.AutoReconnect" — but without the explicit 'AutoClient' argument — plus
'GetClient' \/ 'GetSession' to recover the held handle and initial 'Session'.
-}
data Mqtt :: Effect where
  GetClient :: Mqtt m AutoClient
  GetSession :: Mqtt m Session
  Status :: Mqtt m Status
  IsConnected :: Mqtt m Bool
  Publish :: Topic -> ByteString -> PublishOptions -> Mqtt m PublishResult
  Publish_ :: Topic -> ByteString -> Mqtt m ()
  Subscribe :: NonEmpty Subscription -> Properties -> Mqtt m (NonEmpty ReasonCode)
  Subscribe1 :: TopicFilter -> QoS -> Mqtt m ReasonCode
  Unsubscribe :: NonEmpty TopicFilter -> Properties -> Mqtt m (NonEmpty ReasonCode)
  Ping :: Mqtt m ()
  Subscriptions :: Mqtt m [Subscription]
  RecvMessage :: Mqtt m Message
  TryRecvMessage :: Mqtt m (Maybe Message)

type instance DispatchOf Mqtt = Dynamic

-- Interpreters --------------------------------------------------------------

{- | Run a 'Mqtt' computation against an already-connected 'AutoClient' and its initial
'Session', delegating every operation to "Network.Mqtt.Client.AutoReconnect" in 'IO'.
-}
runMqttWith :: (IOE :> es) => AutoClient -> Session -> Eff (Mqtt : es) a -> Eff es a
runMqttWith ac session = interpret_ \case
  GetClient -> pure ac
  GetSession -> pure session
  Status -> liftIO (Auto.status ac)
  IsConnected -> liftIO (Auto.isConnected ac)
  Publish top body opts -> liftIO (Auto.publish ac top body opts)
  Publish_ top body -> liftIO (Auto.publish_ ac top body)
  Subscribe subs props -> liftIO (Auto.subscribe ac subs props)
  Subscribe1 tf q -> liftIO (Auto.subscribe1 ac tf q)
  Unsubscribe fs props -> liftIO (Auto.unsubscribe ac fs props)
  Ping -> liftIO (Auto.ping ac)
  Subscriptions -> liftIO (Auto.subscriptions ac)
  RecvMessage -> liftIO (Auto.recvMessage ac)
  TryRecvMessage -> liftIO (Auto.tryRecvMessage ac)

{- | 'connect' and then 'runMqttWith'. Connects and runs the computation, but does __not__
disconnect afterwards (the supervisor keeps the link alive) — suitable for a long-running
process. For bracketed teardown use @'withClient' opts cfg \\ac s -> 'runMqttWith' ac s …@.
-}
runMqtt :: (IOE :> es) => ConnectOptions -> AutoReconnectConfig -> Eff (Mqtt : es) a -> Eff es a
runMqtt opts cfg act = do
  (ac, session) <- connect opts cfg
  runMqttWith ac session act

-- Acquiring a client --------------------------------------------------------

-- | Connect and start the reconnect supervisor (wraps @connect@ of "Network.Mqtt.Client.AutoReconnect").
connect :: (IOE :> es) => ConnectOptions -> AutoReconnectConfig -> Eff es (AutoClient, Session)
connect opts cfg = liftIO (Auto.connect opts cfg)

{- | 'connect' \/ disconnect bracketed around an action (wraps @withClient@). This is the only
teardown path this module exposes.
-}
withClient ::
  (IOE :> es) =>
  ConnectOptions ->
  AutoReconnectConfig ->
  (AutoClient -> Session -> Eff es a) ->
  Eff es a
withClient opts cfg act =
  withSeqEffToIO \unlift ->
    Auto.withClient opts cfg \ac session -> unlift (act ac session)

-- The held client & session -------------------------------------------------

-- | The 'AutoClient' the interpreter is operating on (e.g. for 'recvMessageSTM' or 'underlying').
getClient :: (Mqtt :> es) => Eff es AutoClient
getClient = send GetClient

-- | The initial handshake 'Session' supplied to 'runMqttWith'.
getSession :: (Mqtt :> es) => Eff es Session
getSession = send GetSession

-- Status --------------------------------------------------------------------

-- | The current connection 'Status' (wraps @status@).
status :: (Mqtt :> es) => Eff es Status
status = send Status

-- | Is the link currently up? (wraps @isConnected@).
isConnected :: (Mqtt :> es) => Eff es Bool
isConnected = send IsConnected

-- Messaging -----------------------------------------------------------------

-- | Publish, blocking until connected (wraps @publish@).
publish :: (Mqtt :> es) => Topic -> ByteString -> PublishOptions -> Eff es PublishResult
publish top body opts = send (Publish top body opts)

-- | Fire-and-forget QoS-0 publish, blocking until connected (wraps @publish_@).
publish_ :: (Mqtt :> es) => Topic -> ByteString -> Eff es ()
publish_ top body = send (Publish_ top body)

-- | Subscribe, blocking until connected; successful filters are tracked for replay (wraps @subscribe@).
subscribe :: (Mqtt :> es) => NonEmpty Subscription -> Properties -> Eff es (NonEmpty ReasonCode)
subscribe subs props = send (Subscribe subs props)

-- | Subscribe to a single filter at the given QoS (wraps @subscribe1@).
subscribe1 :: (Mqtt :> es) => TopicFilter -> QoS -> Eff es ReasonCode
subscribe1 tf q = send (Subscribe1 tf q)

-- | Unsubscribe, blocking until connected; successful filters are dropped from the tracked set (wraps @unsubscribe@).
unsubscribe :: (Mqtt :> es) => NonEmpty TopicFilter -> Properties -> Eff es (NonEmpty ReasonCode)
unsubscribe fs props = send (Unsubscribe fs props)

-- | Send a PINGREQ and wait for the PINGRESP, blocking until connected (wraps @ping@).
ping :: (Mqtt :> es) => Eff es ()
ping = send Ping

-- | A snapshot of the currently-tracked subscriptions (wraps @subscriptions@).
subscriptions :: (Mqtt :> es) => Eff es [Subscription]
subscriptions = send Subscriptions

-- Receiving -----------------------------------------------------------------

-- | Block for the next message; the queue is reused across reconnects (wraps @recvMessage@).
recvMessage :: (Mqtt :> es) => Eff es Message
recvMessage = send RecvMessage

-- | Non-blocking variant of 'recvMessage' (wraps @tryRecvMessage@).
tryRecvMessage :: (Mqtt :> es) => Eff es (Maybe Message)
tryRecvMessage = send TryRecvMessage

-- Note: 'recvMessageSTM' is re-exported unchanged from "Network.Mqtt.Client.AutoReconnect".
-- It is pure (it only /constructs/ an @STM Message@), so it needs no lifting; obtain the
-- client with 'getClient' and run the action with an STM-capable effect, e.g.
-- @Effectful.Concurrent.STM.atomically@.
