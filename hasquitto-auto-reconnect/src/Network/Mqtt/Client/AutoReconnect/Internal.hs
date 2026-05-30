{- | An auto-reconnecting, auto-resubscribing wrapper around the low-level
"Network.Mqtt.Client" engine, built entirely on its public API.

A background /supervisor/ thread watches the underlying connection; when it drops
unexpectedly it re-establishes it with full-jitter exponential backoff and replays
the tracked subscriptions. Messaging operations /block until reconnected/ rather
than failing during the reconnect window; a call already in flight when the link
drops surfaces its connection-lost error to that one caller (the core engine
replays in-flight QoS 1\/2 publishes itself, but only on a /resumed/ session — see
"Network.Mqtt.Client.AutoReconnect" for the delivery caveats).

The public surface is re-exported by "Network.Mqtt.Client.AutoReconnect".
-}
module Network.Mqtt.Client.AutoReconnect.Internal (
  -- * Configuration
  BackoffConfig (..),
  defaultBackoffConfig,
  AutoReconnectConfig (..),
  defaultAutoReconnectConfig,
  ConnectOptions (..),
  defaultConnectOptions,
  PublishOptions (..),
  defaultPublishOptions,
  PublishResult (..),
  Session (..),
  Message (..),

  -- * Handle & status
  AutoClient,
  Status (..),
  underlying,

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
  recvMessage,
  tryRecvMessage,
  recvMessageSTM,

  -- * Internals (exposed for white-box testing)
  backoffBound,
  recordSubscriptions,
  removeSubscriptions,
  defaultSubscription,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException, toException)
import Control.Exception.Safe (bracket, catchAny, tryAny)
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.Mqtt.Client (Client, ConnectOptions (..), PublishOptions (..), PublishResult (..), Session (..), defaultConnectOptions, defaultPublishOptions)
import Network.Mqtt.Client qualified as Mqtt
import Network.Mqtt.Exception (ConnectionError (..), MqttException (..))
import Network.Mqtt.Message (Message (..))
import Network.Mqtt.Types (
  Properties,
  QoS,
  ReasonCode,
  RetainHandling (SendOnSubscribe),
  Subscription (..),
  Topic,
  TopicFilter,
  isError,
  pattern NormalDisconnection,
 )
import System.Random (StdGen, initStdGen, randomR)

-- Configuration -------------------------------------------------------------

{- | Exponential-backoff bounds, in microseconds. The /n/-th reconnect attempt
draws its delay uniformly from @[0, min maxDelay (baseDelay * 2^(n-1))]@ (full
jitter). Negative values are treated as @0@.
-}
data BackoffConfig = BackoffConfig
  { baseDelay :: !Int
  , maxDelay :: !Int
  }
  deriving stock (Show, Eq, Generic)

-- | 100 ms base, capped at 30 s.
defaultBackoffConfig :: BackoffConfig
defaultBackoffConfig = BackoffConfig {baseDelay = 100_000, maxDelay = 30_000_000}

-- | How the supervisor reconnects and what it does afterwards.
data AutoReconnectConfig = AutoReconnectConfig
  { backoff :: !BackoffConfig
  , maxRetries :: !(Maybe Int)
  {- ^ Give up after this many /consecutive/ failed attempts; 'Nothing' = retry
  forever.
  -}
  , resubscribe :: !Bool
  -- ^ Replay the tracked subscriptions after a /fresh/-session reconnect.
  , onReconnect :: !(Maybe (AutoClient -> Session -> IO ()))
  {- ^ Run after each successful reconnect, on a detached thread that is /not/
  cancelled by 'disconnect' (its lifetime is the caller's to manage).
  -}
  , onResubscribe :: !(Maybe ([(Subscription, ReasonCode)] -> IO ()))
  {- ^ Receive the per-filter results of an automatic resubscribe, on a detached
  thread that is /not/ cancelled by 'disconnect'.
  -}
  }
  deriving stock (Generic)

{- | Sensible defaults: 'defaultBackoffConfig', retry forever, resubscribe on a
fresh session, no callbacks.
-}
defaultAutoReconnectConfig :: AutoReconnectConfig
defaultAutoReconnectConfig =
  AutoReconnectConfig
    { backoff = defaultBackoffConfig
    , maxRetries = Nothing
    , resubscribe = True
    , onReconnect = Nothing
    , onResubscribe = Nothing
    }

-- Handle --------------------------------------------------------------------

-- | The connection lifecycle as seen by the wrapper.
data Status
  = -- | The link is up; operations proceed.
    Connected
  | -- | The link dropped; the supervisor is re-establishing it. Operations block.
    Reconnecting
  | -- | Permanently closed (intentional 'disconnect' or retries exhausted).
    Closed
  deriving stock (Show, Eq, Generic)

-- | An opaque auto-reconnecting client handle.
data AutoClient = AutoClient
  { client :: !Client
  , config :: !AutoReconnectConfig
  , registry :: !(TVar (Map TopicFilter Subscription))
  , state :: !(TVar Status)
  , closedResult :: !(TMVar (Either MqttException ReasonCode))
  , gen :: !(IORef StdGen)
  , supervisor :: !(MVar (Async ()))
  }

-- | The underlying low-level client (an escape hatch for raw operations).
underlying :: AutoClient -> Client
underlying ac = ac.client

-- Lifecycle -----------------------------------------------------------------

{- | Connect and start the reconnect supervisor. The initial 'Session' is the
result of the first handshake (the supervisor only acts on later drops).
-}
connect :: ConnectOptions -> AutoReconnectConfig -> IO (AutoClient, Session)
connect opts cfg = do
  (c, session) <- Mqtt.connect opts
  reg <- newTVarIO Map.empty
  st <- newTVarIO Connected
  cr <- newEmptyTMVarIO
  g <- initStdGen >>= newIORef
  sup <- newEmptyMVar
  let ac =
        AutoClient
          { client = c
          , config = cfg
          , registry = reg
          , state = st
          , closedResult = cr
          , gen = g
          , supervisor = sup
          }
  a <- async (supervisorLoop ac)
  putMVar sup a
  pure (ac, session)

-- | 'connect' \/ 'disconnect' bracketed around an action.
withClient :: ConnectOptions -> AutoReconnectConfig -> (AutoClient -> Session -> IO a) -> IO a
withClient opts cfg act =
  bracket (connect opts cfg) (disconnect . fst) (uncurry act)

{- | Disconnect intentionally: stop the supervisor first (so it can never observe
the close and reconnect), then close the underlying client, then mark the wrapper
permanently 'Closed'.
-}
disconnect :: AutoClient -> IO ()
disconnect ac = do
  a <- readMVar ac.supervisor
  cancel a
  Mqtt.disconnect ac.client
  finalize ac (Right NormalDisconnection)

{- | Block until the client is /permanently/ closed (intentional 'disconnect' or
exhausted retries), reporting why. Transient drops that the supervisor recovers do
not return here.
-}
waitClosed :: AutoClient -> IO (Either MqttException ReasonCode)
waitClosed ac = atomically do
  s <- readTVar ac.state
  case s of
    Closed -> readTMVar ac.closedResult
    _ -> retry

-- | The current connection 'Status'.
status :: AutoClient -> IO Status
status ac = readTVarIO ac.state

{- | Is the link currently up? Reflects the /actual/ connection, not just the
wrapper's view: the wrapper @state@ lags the core's own close signal by the
supervisor's reaction time, so we also consult the underlying client (mirroring
'withConnected').
-}
isConnected :: AutoClient -> IO Bool
isConnected ac = do
  s <- readTVarIO ac.state
  live <- Mqtt.isConnected ac.client
  pure (s == Connected && live)

-- Messaging -----------------------------------------------------------------

-- | Publish, blocking until connected. See 'withConnected' for the in-flight rule.
publish :: AutoClient -> Topic -> ByteString -> PublishOptions -> IO PublishResult
publish ac top body opts = withConnected ac \c -> Mqtt.publish c top body opts

-- | Fire-and-forget QoS-0 publish, blocking until connected.
publish_ :: AutoClient -> Topic -> ByteString -> IO ()
publish_ ac top body = withConnected ac \c -> Mqtt.publish_ c top body

{- | Subscribe, blocking until connected. Filters whose SUBACK reason code is a
success are recorded so they can be replayed after a fresh-session reconnect.
-}
subscribe :: AutoClient -> NonEmpty Subscription -> Properties -> IO (NonEmpty ReasonCode)
subscribe ac subs props = withConnected ac \c -> do
  rcs <- Mqtt.subscribe c subs props
  atomically (modifyTVar' ac.registry (recordSubscriptions (NE.toList (NE.zip subs rcs))))
  pure rcs

-- | Subscribe to a single filter at the given QoS (default subscription options).
subscribe1 :: AutoClient -> TopicFilter -> QoS -> IO ReasonCode
subscribe1 ac tf q = NE.head <$> subscribe ac (pure (defaultSubscription tf q)) []

{- | Unsubscribe, blocking until connected. Filters whose UNSUBACK reason code is a
success are dropped from the tracked set.
-}
unsubscribe :: AutoClient -> NonEmpty TopicFilter -> Properties -> IO (NonEmpty ReasonCode)
unsubscribe ac fs props = withConnected ac \c -> do
  rcs <- Mqtt.unsubscribe c fs props
  atomically (modifyTVar' ac.registry (removeSubscriptions (NE.toList (NE.zip fs rcs))))
  pure rcs

-- | Send a PINGREQ and wait for the PINGRESP, blocking until connected.
ping :: AutoClient -> IO ()
ping ac = withConnected ac Mqtt.ping

-- | A snapshot of the currently-tracked subscriptions.
subscriptions :: AutoClient -> IO [Subscription]
subscriptions ac = Map.elems <$> readTVarIO ac.registry

{- | Block for the next message. Delegates straight to the underlying client (the
queue is reused across reconnects, so a consumer loop spans them). Note: a QoS 1\/2
message received-but-unconsumed before a /fresh/-session reconnect carries a
stale-ack caveat — see "Network.Mqtt.Client.AutoReconnect".
-}
recvMessage :: AutoClient -> IO Message
recvMessage ac = Mqtt.recvMessage ac.client

-- | Non-blocking variant of 'recvMessage'.
tryRecvMessage :: AutoClient -> IO (Maybe Message)
tryRecvMessage ac = Mqtt.tryRecvMessage ac.client

-- | The STM-composable variant of 'recvMessage'.
recvMessageSTM :: AutoClient -> STM Message
recvMessageSTM ac = Mqtt.recvMessageSTM ac.client

{- | Run an action against the underlying client once the link is genuinely up.

Blocks (no busy-wait) while the wrapper is 'Reconnecting'; throws if it is
permanently 'Closed'. The wrapper @state@ lags the core's own close signal by the
supervisor's reaction time, so after observing 'Connected' we also confirm the core
link is actually open; if the core dropped but the supervisor has not flipped the
state yet, we block until it does and re-await. Once @act@ begins, errors propagate
to the caller (the in-flight rule).
-}
withConnected :: AutoClient -> (Client -> IO a) -> IO a
withConnected ac act = awaitUp >> act ac.client
  where
    awaitUp = do
      atomically do
        s <- readTVar ac.state
        case s of
          Connected -> pure ()
          Reconnecting -> retry
          Closed -> readTMVar ac.closedResult >>= throwSTM . closedException
      live <- Mqtt.isConnected ac.client
      unless live do
        atomically do
          s <- readTVar ac.state
          case s of
            Connected -> retry -- core dropped; wait for the supervisor to flip the state
            _ -> pure ()
        awaitUp

closedException :: Either MqttException ReasonCode -> MqttException
closedException = \case
  Left e -> e
  Right _ -> TransportClosed ConnectionClosed

-- Supervisor ----------------------------------------------------------------

supervisorLoop :: AutoClient -> IO ()
supervisorLoop ac = loop
  where
    loop = do
      _ <- Mqtt.waitClosed ac.client -- block until the underlying link closes
      atomically (writeTVar ac.state Reconnecting)
      r <- reconnectWithBackoff ac
      case r of
        Right session -> do
          atomically (writeTVar ac.state Connected)
          -- Fork the hook on a detached thread so a slow/throwing hook can neither
          -- wedge the supervisor (which would livelock 'withConnected') nor kill it.
          _ <- async (runOnReconnect session `catchAny` \_ -> pure ())
          loop
        Left e -> finalize ac (Left (coarsen e)) -- retries exhausted → permanent close
    runOnReconnect session = maybe (pure ()) (\f -> f ac session) ac.config.onReconnect

{- | Retry @reconnect@ with full-jitter backoff (delay applied /before/ each
attempt). 'tryAny' catches only synchronous failures (e.g. an 'IOException' from a
down broker) and retries them; an asynchronous 'AsyncCancelled' from 'disconnect'
propagates out and ends the supervisor.
-}
reconnectWithBackoff :: AutoClient -> IO (Either SomeException Session)
reconnectWithBackoff ac = go 1 Nothing
  where
    cfg = ac.config
    go attempt mlast = case cfg.maxRetries of
      Just m | attempt > m -> pure (Left (fromMaybe defaultErr mlast))
      _ -> do
        threadDelay =<< fullJitterDelay ac attempt
        r <- tryAny (attemptReconnect ac)
        case r of
          Right s -> pure (Right s)
          Left e -> go (attempt + 1) (Just e)
    defaultErr = toException (TransportClosed ConnectionClosed)

-- | One reconnect attempt: re-handshake, then (on a fresh session) replay subscriptions.
attemptReconnect :: AutoClient -> IO Session
attemptReconnect ac = do
  session <- Mqtt.reconnect ac.client
  when (ac.config.resubscribe && not session.sessionPresent) (resubscribeAll ac)
  pure session

{- | Replay the tracked subscriptions in a single SUBSCRIBE. A thrown SUBSCRIBE
(the link dropped again) propagates and fails the attempt; a SUBACK with per-filter
errors does not — rejected filters are dropped and the results reported (so we never
loop forever on a permanently-rejected filter).
-}
resubscribeAll :: AutoClient -> IO ()
resubscribeAll ac = do
  m <- readTVarIO ac.registry
  case NE.nonEmpty (Map.elems m) of
    Nothing -> pure ()
    Just subs -> do
      rcs <- Mqtt.subscribe ac.client subs []
      let results = NE.toList (NE.zip subs rcs)
          failed = [s.topicFilter | (s, rc) <- results, isError rc]
      atomically (modifyTVar' ac.registry (\mp -> foldl' (flip Map.delete) mp failed))
      mapM_ (\cb -> void (async (cb results `catchAny` \_ -> pure ()))) ac.config.onResubscribe

-- | Mark the wrapper permanently 'Closed'. Idempotent; the first reason wins.
finalize :: AutoClient -> Either MqttException ReasonCode -> IO ()
finalize ac r = atomically do
  s <- readTVar ac.state
  case s of
    Closed -> pure ()
    _ -> do
      writeTVar ac.state Closed
      void (tryPutTMVar ac.closedResult r)

-- | Normalize a give-up cause to the public exception type.
coarsen :: SomeException -> MqttException
coarsen e = fromMaybe (TransportClosed ConnectionClosed) (fromException e)

-- Backoff -------------------------------------------------------------------

{- | The full-jitter /upper bound/ in microseconds for a (1-based) attempt:
@min maxDelay (baseDelay * 2^(attempt-1))@. The product is computed in 'Integer'
(so it cannot overflow before the cap is applied) and the exponent is clamped at 30;
negative inputs are treated as @0@.
-}
backoffBound :: BackoffConfig -> Int -> Int
backoffBound cfg attempt =
  let s = min 30 (max 0 (attempt - 1))
      raw = toInteger (max 0 cfg.baseDelay) * (2 ^ s)
   in fromInteger (min (toInteger (max 0 cfg.maxDelay)) raw)

-- | Draw a full-jitter delay in @[0, 'backoffBound']@ from the per-client RNG.
fullJitterDelay :: AutoClient -> Int -> IO Int
fullJitterDelay ac attempt =
  atomicModifyIORef' ac.gen \g ->
    let (d, g') = randomR (0, backoffBound ac.config.backoff attempt) g
     in (g', d)

-- Subscription registry helpers ---------------------------------------------

-- | Record the requested subscriptions whose reason code is a success.
recordSubscriptions ::
  [(Subscription, ReasonCode)] ->
  Map TopicFilter Subscription ->
  Map TopicFilter Subscription
recordSubscriptions prs m0 =
  foldl' (\m (s, rc) -> if isError rc then m else Map.insert s.topicFilter s m) m0 prs

-- | Drop the filters whose (unsubscribe) reason code is a success.
removeSubscriptions ::
  [(TopicFilter, ReasonCode)] ->
  Map TopicFilter Subscription ->
  Map TopicFilter Subscription
removeSubscriptions prs m0 =
  foldl' (\m (f, rc) -> if isError rc then m else Map.delete f m) m0 prs

-- | The 'Subscription' that 'subscribe1' requests (matching the core's defaults).
defaultSubscription :: TopicFilter -> QoS -> Subscription
defaultSubscription tf q =
  Subscription
    { topicFilter = tf
    , qos = q
    , noLocal = False
    , retainAsPublished = False
    , retainHandling = SendOnSubscribe
    }
