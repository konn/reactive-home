{- | A thin [@effectful@](https://hackage.haskell.org/package/effectful) wrapper over
"Network.Mqtt.Client.AutoReconnect": a dynamic, IO-backed 'Mqtt' effect whose operations
mirror the auto-reconnecting MQTT v5 client one-for-one. Every @… -> IO x@ in that module
becomes @('Mqtt' ':>' es) => … -> 'Eff' es x@; run the effect with 'runMqtt' (which needs
'IOE'). Intended for qualified import:

@
import Effectful.Network.Mqtt qualified as Mqtt
@

The handle type ('AutoClient'), the configuration ('AutoReconnectConfig', 'BackoffConfig'),
the protocol vocabulary ("Network.Mqtt.Types"), the received-message type
("Network.Mqtt.Message"), and the exception hierarchy ("Network.Mqtt.Exception") are
re-exported here, so a caller needs to import only this module. The blocking behaviour,
delivery semantics, and reconnect caveats are exactly those of
"Network.Mqtt.Client.AutoReconnect" — read its Haddock.

The 'Mqtt' constructors are exported so you can write your own interpreter (for example a
mock for tests); ordinary code only needs the smart constructors and 'runMqtt'.
-}
module Effectful.Network.Mqtt (
  -- * The effect
  Mqtt (..),
  runMqtt,

  -- * Lifecycle
  connect,
  withClient,
  disconnect,
  waitClosed,
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
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, liftIO, type (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
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

{- | The MQTT capability: an auto-reconnecting MQTT v5 client expressed as operations in
the 'Eff' monad. Each constructor corresponds 1:1 to an operation of
"Network.Mqtt.Client.AutoReconnect" and carries its explicit 'AutoClient'. Interpret with
'runMqtt'.
-}
data Mqtt :: Effect where
  Connect :: ConnectOptions -> AutoReconnectConfig -> Mqtt m (AutoClient, Session)
  WithClient :: ConnectOptions -> AutoReconnectConfig -> (AutoClient -> Session -> m a) -> Mqtt m a
  Disconnect :: AutoClient -> Mqtt m ()
  WaitClosed :: AutoClient -> Mqtt m (Either MqttException ReasonCode)
  Status :: AutoClient -> Mqtt m Status
  IsConnected :: AutoClient -> Mqtt m Bool
  Publish :: AutoClient -> Topic -> ByteString -> PublishOptions -> Mqtt m PublishResult
  Publish_ :: AutoClient -> Topic -> ByteString -> Mqtt m ()
  Subscribe :: AutoClient -> NonEmpty Subscription -> Properties -> Mqtt m (NonEmpty ReasonCode)
  Subscribe1 :: AutoClient -> TopicFilter -> QoS -> Mqtt m ReasonCode
  Unsubscribe :: AutoClient -> NonEmpty TopicFilter -> Properties -> Mqtt m (NonEmpty ReasonCode)
  Ping :: AutoClient -> Mqtt m ()
  Subscriptions :: AutoClient -> Mqtt m [Subscription]
  RecvMessage :: AutoClient -> Mqtt m Message
  TryRecvMessage :: AutoClient -> Mqtt m (Maybe Message)

type instance DispatchOf Mqtt = Dynamic

-- Interpreter ---------------------------------------------------------------

{- | Interpret 'Mqtt' by delegating every operation to "Network.Mqtt.Client.AutoReconnect"
in 'IO'. The single higher-order operation ('withClient') runs its continuation back in
'Eff' via 'localSeqUnliftIO', so the underlying bracket still owns the connection lifetime.
-}
runMqtt :: (IOE :> es) => Eff (Mqtt : es) a -> Eff es a
runMqtt = interpret \env -> \case
  Connect opts cfg -> liftIO (Auto.connect opts cfg)
  WithClient opts cfg act ->
    localSeqUnliftIO env \unlift ->
      Auto.withClient opts cfg \ac session -> unlift (act ac session)
  Disconnect ac -> liftIO (Auto.disconnect ac)
  WaitClosed ac -> liftIO (Auto.waitClosed ac)
  Status ac -> liftIO (Auto.status ac)
  IsConnected ac -> liftIO (Auto.isConnected ac)
  Publish ac top body opts -> liftIO (Auto.publish ac top body opts)
  Publish_ ac top body -> liftIO (Auto.publish_ ac top body)
  Subscribe ac subs props -> liftIO (Auto.subscribe ac subs props)
  Subscribe1 ac tf q -> liftIO (Auto.subscribe1 ac tf q)
  Unsubscribe ac fs props -> liftIO (Auto.unsubscribe ac fs props)
  Ping ac -> liftIO (Auto.ping ac)
  Subscriptions ac -> liftIO (Auto.subscriptions ac)
  RecvMessage ac -> liftIO (Auto.recvMessage ac)
  TryRecvMessage ac -> liftIO (Auto.tryRecvMessage ac)

-- Lifecycle -----------------------------------------------------------------

-- | Connect and start the reconnect supervisor (wraps @connect@ of "Network.Mqtt.Client.AutoReconnect").
connect :: (Mqtt :> es) => ConnectOptions -> AutoReconnectConfig -> Eff es (AutoClient, Session)
connect opts cfg = send (Connect opts cfg)

-- | 'connect' \/ 'disconnect' bracketed around an action (wraps @withClient@).
withClient ::
  (Mqtt :> es) =>
  ConnectOptions ->
  AutoReconnectConfig ->
  (AutoClient -> Session -> Eff es a) ->
  Eff es a
withClient opts cfg act = send (WithClient opts cfg act)

-- | Disconnect intentionally; the wrapper becomes permanently closed (wraps @disconnect@).
disconnect :: (Mqtt :> es) => AutoClient -> Eff es ()
disconnect ac = send (Disconnect ac)

-- | Block until the client is permanently closed, reporting why (wraps @waitClosed@).
waitClosed :: (Mqtt :> es) => AutoClient -> Eff es (Either MqttException ReasonCode)
waitClosed ac = send (WaitClosed ac)

-- | The current connection 'Status' (wraps @status@).
status :: (Mqtt :> es) => AutoClient -> Eff es Status
status ac = send (Status ac)

-- | Is the link currently up? (wraps @isConnected@).
isConnected :: (Mqtt :> es) => AutoClient -> Eff es Bool
isConnected ac = send (IsConnected ac)

-- Messaging -----------------------------------------------------------------

-- | Publish, blocking until connected (wraps @publish@).
publish :: (Mqtt :> es) => AutoClient -> Topic -> ByteString -> PublishOptions -> Eff es PublishResult
publish ac top body opts = send (Publish ac top body opts)

-- | Fire-and-forget QoS-0 publish, blocking until connected (wraps @publish_@).
publish_ :: (Mqtt :> es) => AutoClient -> Topic -> ByteString -> Eff es ()
publish_ ac top body = send (Publish_ ac top body)

-- | Subscribe, blocking until connected; successful filters are tracked for replay (wraps @subscribe@).
subscribe :: (Mqtt :> es) => AutoClient -> NonEmpty Subscription -> Properties -> Eff es (NonEmpty ReasonCode)
subscribe ac subs props = send (Subscribe ac subs props)

-- | Subscribe to a single filter at the given QoS (wraps @subscribe1@).
subscribe1 :: (Mqtt :> es) => AutoClient -> TopicFilter -> QoS -> Eff es ReasonCode
subscribe1 ac tf q = send (Subscribe1 ac tf q)

-- | Unsubscribe, blocking until connected; successful filters are dropped from the tracked set (wraps @unsubscribe@).
unsubscribe :: (Mqtt :> es) => AutoClient -> NonEmpty TopicFilter -> Properties -> Eff es (NonEmpty ReasonCode)
unsubscribe ac fs props = send (Unsubscribe ac fs props)

-- | Send a PINGREQ and wait for the PINGRESP, blocking until connected (wraps @ping@).
ping :: (Mqtt :> es) => AutoClient -> Eff es ()
ping ac = send (Ping ac)

-- | A snapshot of the currently-tracked subscriptions (wraps @subscriptions@).
subscriptions :: (Mqtt :> es) => AutoClient -> Eff es [Subscription]
subscriptions ac = send (Subscriptions ac)

-- Receiving -----------------------------------------------------------------

-- | Block for the next message; the queue is reused across reconnects (wraps @recvMessage@).
recvMessage :: (Mqtt :> es) => AutoClient -> Eff es Message
recvMessage ac = send (RecvMessage ac)

-- | Non-blocking variant of 'recvMessage' (wraps @tryRecvMessage@).
tryRecvMessage :: (Mqtt :> es) => AutoClient -> Eff es (Maybe Message)
tryRecvMessage ac = send (TryRecvMessage ac)

-- Note: 'recvMessageSTM' is re-exported unchanged from "Network.Mqtt.Client.AutoReconnect".
-- It is pure (it only /constructs/ an @STM Message@), so it needs no lifting; run the action
-- with an STM-capable effect, e.g. @Effectful.Concurrent.STM.atomically@.
