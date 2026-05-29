{- | The MQTT client engine: the connection handshake, the reader\/keepalive\/
ack-writer threads, packet-identifier allocation, the request correlation map,
the QoS 1\/2 state machines (with acknowledge-on-consume backpressure), CONNACK
capability gates, bidirectional Topic Alias, enhanced authentication, and the
public operations. The public surface is re-exported by "Network.Mqtt.Client".
-}
module Network.Mqtt.Client.Internal (
  -- * Configuration
  ConnectOptions (..),
  defaultConnectOptions,
  OverflowPolicy (..),
  TopicAliasMode (..),
  Authenticator (..),
  AuthChallenge (..),
  AuthResponse (..),
  PublishOptions (..),
  defaultPublishOptions,
  PublishResult (..),

  -- * Handle & session
  Client,
  Session (..),

  -- * Lifecycle
  connect,
  withClient,
  disconnect,
  disconnectWith,
  reconnect,
  waitClosed,
  isConnected,

  -- * Messaging
  publish,
  publish_,
  subscribe,
  subscribe1,
  unsubscribe,
  recvMessage,
  tryRecvMessage,
  recvMessageSTM,
  ping,
  reauthenticate,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, AsyncCancelled, async, cancel)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception.Safe (Handler (..), SomeException, bracket, bracket_, catch, catches, finally, mask, onException, throwIO)
import Control.Monad (forever, unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word16, Word32)
import ListT qualified
import Network.Mqtt.Codec (encodePacketBS)
import Network.Mqtt.Connection.Internal
import Network.Mqtt.Exception
import Network.Mqtt.Message (Message (..))
import Network.Mqtt.Types
import StmContainers.Map qualified as SM
import System.Timeout (timeout)

-- Configuration -------------------------------------------------------------

-- | How a full inbound QoS-0 queue behaves (QoS 0 has no flow control).
data OverflowPolicy = DropNewest | DropOldest | Block
  deriving stock (Show, Eq)

-- | Outbound Topic Alias policy.
data TopicAliasMode = NoAliasing | AliasUpTo !Word16
  deriving stock (Show, Eq)

-- | An enhanced-authentication challenge handed to the 'Authenticator'.
data AuthChallenge = AuthChallenge
  { reasonCode :: !ReasonCode
  , authData :: !(Maybe ByteString)
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

{- | The 'Authenticator' response: the next Authentication Data plus any extra
properties.
-}
data AuthResponse = AuthResponse
  { authData :: !(Maybe ByteString)
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

{- | Enhanced authentication. 'method' goes in the CONNECT Authentication Method
property; 'step' answers each server @0x18@ (Continue authentication) challenge.
-}
data Authenticator = Authenticator
  { method :: !Text
  , initial :: !(Maybe ByteString)
  , step :: !(AuthChallenge -> IO AuthResponse)
  }

-- | Options for establishing a client.
data ConnectOptions = ConnectOptions
  { connectionFactory :: !(IO Conn)
  , clientId :: !Text
  , cleanStart :: !Bool
  , keepAlive :: !Word16
  , username :: !(Maybe Text)
  , password :: !(Maybe ByteString)
  , will :: !(Maybe Will)
  , properties :: !Properties
  , authenticator :: !(Maybe Authenticator)
  , topicAliasSending :: !TopicAliasMode
  , receiveQueueBound :: !Int
  , overflowPolicy :: !OverflowPolicy
  }

{- | Sensible defaults: clean start, 60s keep-alive, no will\/auth, a 1024-deep
inbound queue, drop-newest QoS-0 overflow, no aliasing.
-}
defaultConnectOptions :: IO Conn -> Text -> ConnectOptions
defaultConnectOptions factory cid =
  ConnectOptions
    { connectionFactory = factory
    , clientId = cid
    , cleanStart = True
    , keepAlive = 60
    , username = Nothing
    , password = Nothing
    , will = Nothing
    , properties = []
    , authenticator = Nothing
    , topicAliasSending = NoAliasing
    , receiveQueueBound = 1024
    , overflowPolicy = DropNewest
    }

-- | Per-publish options.
data PublishOptions = PublishOptions
  { qos :: !QoS
  , retain :: !Bool
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | QoS 0, no retain, no properties.
defaultPublishOptions :: PublishOptions
defaultPublishOptions = PublishOptions {qos = QoS0, retain = False, properties = []}

-- | The outcome of a 'publish', shaped by the QoS.
data PublishResult
  = PublishedQoS0
  | AckedQoS1 !ReasonCode !Properties
  | AckedQoS2 !ReasonCode !Properties
  deriving stock (Show, Eq)

-- | The result of the CONNECT\/CONNACK handshake.
data Session = Session
  { sessionPresent :: !Bool
  , reasonCode :: !ReasonCode
  , assignedClientId :: !(Maybe Text)
  , serverProperties :: !Properties
  }
  deriving stock (Show, Eq)

-- Internal state ------------------------------------------------------------

-- | Negotiated server capabilities, cached from CONNACK.
data ServerCaps = ServerCaps
  { receiveMax :: !Word16
  , maximumQoS :: !QoS
  , retainAvailable :: !Bool
  , maxPacketSize :: !(Maybe Word32)
  , topicAliasMax :: !Word16
  , wildcardAvailable :: !Bool
  , subIdAvailable :: !Bool
  , sharedAvailable :: !Bool
  }

data PidPool = PidPool !Word16 !IntSet

data PendingResult
  = RSubAck !SubAckPacket
  | RUnsubAck !SubAckPacket
  | RPubAck !PubAckPacket
  | RPubRec !PubAckPacket
  | RPubComp !PubAckPacket
  | RConnectionLost

data OutboundState
  = OutQoS1 !PublishPacket
  | OutQoS2Publish !PublishPacket
  | OutQoS2Rel

data InboundQoS2 = AwaitingPubRel !Message | AwaitingConsume

data Inbound = Inbound
  { message :: !Message
  , onConsume :: !(Maybe Packet)
  , qos2Pid :: !(Maybe Word16)
  }

data AliasOut = AliasOut !Word16 !(Map Text Word16)

data CloseReason
  = ClosedNormally
  | ClosedByServer !ReasonCode !Properties
  | ClosedByException !MqttException

data ClientState = ClientState
  { connVar :: !(TVar Conn)
  , caps :: !(TVar ServerCaps)
  , writeLock :: !(MVar ())
  , pidPool :: !(TVar PidPool)
  , pending :: !(SM.Map Word16 (TMVar PendingResult))
  , incoming :: !(TBQueue Inbound)
  , ackQueue :: !(TQueue Packet)
  , inQoS2 :: !(SM.Map Word16 InboundQoS2)
  , aliasIn :: !(SM.Map Word16 Topic)
  , aliasOut :: !(TVar AliasOut)
  , outInflight :: !(SM.Map Word16 OutboundState)
  , sendQuota :: !(TVar Int)
  -- ^ Remaining outbound QoS>0 send quota (the server's Receive Maximum).
  , lastSent :: !(TVar UTCTime)
  , lastRecv :: !(TVar UTCTime)
  , pingOut :: !(TVar Bool)
  , pingWaiters :: !(TVar [TMVar ()])
  , reauthActive :: !(TVar Bool)
  , authVar :: !(TMVar AuthPacket)
  , closedVar :: !(TMVar CloseReason)
  , threads :: !(TVar [Async ()])
  , options :: !ConnectOptions
  , inboundMax :: !MaxPacketSize
  , keepAliveSecs :: !(TVar Word16)
  }

-- | An opaque MQTT client handle.
newtype Client = Client ClientState

-- Lifecycle -----------------------------------------------------------------

{- | Establish a connection: build the transport, run the synchronous
CONNECT\/AUTH\/CONNACK handshake, then start the background threads.
-}
connect :: ConnectOptions -> IO (Client, Session)
connect opts = do
  conn <- opts.connectionFactory
  (session, sc, eka) <-
    handshake conn opts `onException` (conn.base.connectionClose `catch` ignoreAll)
  st <- newClientState conn opts sc eka
  startThreads st
  pure (Client st, session)

-- | 'connect' \/ 'disconnect' bracketed around an action.
withClient :: ConnectOptions -> (Client -> Session -> IO a) -> IO a
withClient opts act =
  bracket (connect opts) (\(c, _) -> disconnect c) (\(c, s) -> act c s)

-- | Disconnect normally (reason @0x00@).
disconnect :: Client -> IO ()
disconnect c = disconnectWith c NormalDisconnection []

-- | Disconnect with a specific reason code and properties.
disconnectWith :: Client -> ReasonCode -> Properties -> IO ()
disconnectWith (Client st) rc props = do
  sendPacket st (Disconnect (DisconnectPacket rc props)) `catch` ignoreAll
  recordClosed st ClosedNormally
  stopThreads st

-- | Block until the connection closes, reporting why.
waitClosed :: Client -> IO (Either MqttException ReasonCode)
waitClosed (Client st) = do
  r <- atomically (readTMVar st.closedVar)
  pure case r of
    ClosedNormally -> Right NormalDisconnection
    ClosedByServer rc _ -> Right rc
    ClosedByException e -> Left e

-- | Is the client still connected?
isConnected :: Client -> IO Bool
isConnected (Client st) = atomically (isEmptyTMVar st.closedVar)

-- Messaging -----------------------------------------------------------------

{- | Publish a message. QoS 0 returns immediately; QoS 1 waits for PUBACK; QoS 2
drives PUBREC\/PUBREL\/PUBCOMP to completion.
-}
publish :: Client -> Topic -> ByteString -> PublishOptions -> IO PublishResult
publish (Client st) top body opts = do
  checkOpen st
  sc <- readTVarIO st.caps
  when (fromEnum opts.qos > fromEnum sc.maximumQoS) $
    throwIO (ProtocolViolation (UnsupportedByServer "QoS exceeds server Maximum QoS"))
  when (opts.retain && not sc.retainAvailable) $
    throwIO (ProtocolViolation (UnsupportedByServer "retain not available"))
  case opts.qos of
    QoS0 -> do
      sendPublish st top (basePublish top body opts Nothing)
      pure PublishedQoS0
    QoS1 -> withQuota st $ withRequest st \pid tmv -> do
      let pkt = basePublish top body opts (Just (PacketId pid))
      -- Record in-flight only after a successful write, so a failed send (e.g.
      -- OversizePacket) leaves no orphan entry and the id is freed normally.
      sendPublish st top pkt
      atomically (SM.insert (OutQoS1 pkt) pid st.outInflight)
      res <- awaitResult st tmv
      case res of
        RPubAck pa -> do
          atomically (SM.delete pid st.outInflight)
          pure (AckedQoS1 pa.reasonCode pa.properties)
        RConnectionLost -> throwIO (TransportClosed ConnectionClosed)
        _ -> throwIO (ProtocolViolation (UnexpectedPacket "expected PUBACK"))
    QoS2 -> withQuota st $ withRequest st \pid tmv -> do
      let pkt = basePublish top body opts (Just (PacketId pid))
      sendPublish st top pkt
      atomically (SM.insert (OutQoS2Publish pkt) pid st.outInflight)
      rec_ <- awaitResult st tmv
      case rec_ of
        RConnectionLost -> throwIO (TransportClosed ConnectionClosed)
        RPubRec pr
          | isError pr.reasonCode -> do
              atomically (SM.delete pid st.outInflight)
              pure (AckedQoS2 pr.reasonCode pr.properties)
          | otherwise -> do
              tmv2 <- atomically do
                t <- newEmptyTMVar
                SM.insert t pid st.pending
                SM.insert OutQoS2Rel pid st.outInflight
                pure t
              sendPacket st (PubRel (PubAckPacket (PacketId pid) Success []))
              comp <- awaitResult st tmv2
              case comp of
                RPubComp pc -> do
                  atomically (SM.delete pid st.outInflight)
                  pure (AckedQoS2 pc.reasonCode pc.properties)
                RConnectionLost -> throwIO (TransportClosed ConnectionClosed)
                _ -> throwIO (ProtocolViolation (UnexpectedPacket "expected PUBCOMP"))
        _ -> throwIO (ProtocolViolation (UnexpectedPacket "expected PUBREC"))

-- | Fire-and-forget QoS-0 publish.
publish_ :: Client -> Topic -> ByteString -> IO ()
publish_ c top body = void (publish c top body defaultPublishOptions)

-- | Subscribe to one or more topic filters; returns the per-filter reason codes.
subscribe :: Client -> NonEmpty Subscription -> Properties -> IO (NonEmpty ReasonCode)
subscribe (Client st) subs props = do
  checkOpen st
  sc <- readTVarIO st.caps
  mapM_ (checkSubscriptionCap sc) (NE.toList subs)
  when (any isSubscriptionIdentifier props && not sc.subIdAvailable) $
    throwIO (ProtocolViolation (UnsupportedByServer "subscription identifiers not available"))
  withRequest st \pid tmv -> do
    sendPacket st (Subscribe (SubscribePacket (PacketId pid) subs props))
    res <- awaitResult st tmv
    case res of
      RSubAck sa -> pure sa.reasonCodes
      RConnectionLost -> throwIO (TransportClosed ConnectionClosed)
      _ -> throwIO (ProtocolViolation (UnexpectedPacket "expected SUBACK"))

-- | Subscribe to a single filter at the given QoS (default subscription options).
subscribe1 :: Client -> TopicFilter -> QoS -> IO ReasonCode
subscribe1 c tf q = NE.head <$> subscribe c (pure sub) []
  where
    sub =
      Subscription
        { topicFilter = tf
        , qos = q
        , noLocal = False
        , retainAsPublished = False
        , retainHandling = SendOnSubscribe
        }

-- | Unsubscribe from topic filters; returns the per-filter reason codes.
unsubscribe :: Client -> NonEmpty TopicFilter -> Properties -> IO (NonEmpty ReasonCode)
unsubscribe (Client st) fs props = do
  checkOpen st
  withRequest st \pid tmv -> do
    sendPacket st (Unsubscribe (UnsubscribePacket (PacketId pid) fs props))
    res <- awaitResult st tmv
    case res of
      RUnsubAck sa -> pure sa.reasonCodes
      RConnectionLost -> throwIO (TransportClosed ConnectionClosed)
      _ -> throwIO (ProtocolViolation (UnexpectedPacket "expected UNSUBACK"))

{- | Block for the next message. Consuming a QoS 1\/2 message triggers its
acknowledgement (acknowledge-on-consume backpressure).
-}
recvMessage :: Client -> IO Message
recvMessage = atomically . recvMessageSTM

-- | Non-blocking variant.
tryRecvMessage :: Client -> IO (Maybe Message)
tryRecvMessage (Client st) = atomically do
  m <- tryReadTBQueue st.incoming
  traverse (consume st) m

{- | The composable STM variant. Consuming posts the acknowledgement to an STM
queue drained by the ack-writer thread, so STM never performs IO.
-}
recvMessageSTM :: Client -> STM Message
recvMessageSTM (Client st) = readTBQueue st.incoming >>= consume st

consume :: ClientState -> Inbound -> STM Message
consume st inb = do
  case inb.onConsume of
    Nothing -> pure ()
    Just ackPkt -> writeTQueue st.ackQueue ackPkt
  case inb.qos2Pid of
    Just pid -> SM.delete pid st.inQoS2
    Nothing -> pure ()
  pure inb.message

-- | Send a PINGREQ and block for the PINGRESP (keep-alive is otherwise automatic).
ping :: Client -> IO ()
ping (Client st) = do
  checkOpen st
  w <- atomically do
    t <- newEmptyTMVar
    modifyTVar' st.pingWaiters (t :)
    writeTVar st.pingOut True
    pure t
  sendPacket st PingReq
  atomically (takeTMVar w)

{- | Initiate a mid-session re-authentication (AUTH @0x19@) and drive it to
completion on the calling thread.
-}
reauthenticate :: Client -> IO ()
reauthenticate (Client st) = do
  checkOpen st
  case st.options.authenticator of
    Nothing -> throwIO (ProtocolViolation (UnexpectedPacket "reauthenticate without authenticator"))
    Just auth -> do
      atomically (writeTVar st.reauthActive True)
      let initProps = AuthenticationMethod auth.method : maybe [] (pure . AuthenticationData) auth.initial
      sendPacket st (Auth (AuthPacket ReAuthenticate initProps))
      driveAuth st auth `finally` atomically (writeTVar st.reauthActive False)

driveAuth :: ClientState -> Authenticator -> IO ()
driveAuth st auth = loop
  where
    loop = do
      a <- atomically (takeTMVar st.authVar)
      case a.reasonCode of
        ContinueAuthentication -> do
          checkAuthMethod auth.method a.properties
          let challenge =
                AuthChallenge
                  { reasonCode = a.reasonCode
                  , authData = findProperty authDataProp a.properties
                  , properties = a.properties
                  }
          resp <- auth.step challenge
          let props =
                AuthenticationMethod auth.method
                  : maybe [] (pure . AuthenticationData) resp.authData
                    <> resp.properties
          sendPacket st (Auth (AuthPacket ContinueAuthentication props))
          loop
        Success -> pure ()
        rc -> throwIO (ProtocolViolation (AuthenticationFailed rc))

{- | Re-run the handshake on a fresh connection, replaying (on @sessionPresent@)
or discarding (otherwise) the outbound in-flight state.
-}
reconnect :: Client -> IO Session
reconnect (Client st) = do
  stopThreads st
  -- Wake any lingering request/ping waiters so they cannot block forever, and
  -- reset per-connection transient state.
  atomically do
    _ <- tryTakeTMVar st.closedVar
    waiters <- ListT.toList (SM.listT st.pending)
    mapM_ (\(_, t) -> void (tryPutTMVar t RConnectionLost)) waiters
    SM.reset st.pending
    ws <- readTVar st.pingWaiters
    mapM_ (\t -> void (tryPutTMVar t ())) ws
    writeTVar st.pingWaiters []
    SM.reset st.aliasIn
    writeTVar st.aliasOut (AliasOut 1 Map.empty)
    writeTVar st.pingOut False
  -- Close the old transport and run the new handshake while holding the write
  -- lock, so no concurrent send can race the connection swap.
  session <- withMVar st.writeLock \_ -> do
    old <- readTVarIO st.connVar
    old.base.connectionClose `catch` ignoreAll
    conn <- st.options.connectionFactory
    (session, sc, eka) <-
      handshake conn st.options `onException` (conn.base.connectionClose `catch` ignoreAll)
    atomically do
      writeTVar st.connVar conn
      writeTVar st.caps sc
      writeTVar st.keepAliveSecs eka
      writeTVar st.sendQuota (fromIntegral sc.receiveMax)
    pure session
  -- On a resumed session keep the surviving outbound in-flight state (its ids
  -- remain reserved in the pool, since they were never freed) and replay it. On a
  -- fresh session discard it and release all ids.
  if session.sessionPresent
    then replayOutbound st
    else atomically do
      SM.reset st.outInflight
      SM.reset st.inQoS2
      writeTVar st.pidPool (PidPool 1 IntSet.empty)
  startThreads st
  pure session

{- | Resend outbound in-flight packets (best effort, fire-and-forget): QoS 1\/2
PUBLISH with DUP=1, PUBREL for the PUBCOMP-awaiting half of QoS 2.
-}
replayOutbound :: ClientState -> IO ()
replayOutbound st = do
  pairs <- atomically (ListT.toList (SM.listT st.outInflight))
  mapM_ resend pairs
  where
    resend (_, OutQoS1 pkt) = sendPacket st (Publish (setDup pkt))
    resend (_, OutQoS2Publish pkt) = sendPacket st (Publish (setDup pkt))
    resend (pid, OutQoS2Rel) = sendPacket st (PubRel (PubAckPacket (PacketId pid) Success []))
    setDup p =
      PublishPacket
        { topic = p.topic
        , packetId = p.packetId
        , qos = p.qos
        , retain = p.retain
        , dup = True
        , payload = p.payload
        , properties = p.properties
        }

-- Handshake -----------------------------------------------------------------

handshakeTimeoutMicros :: Int
handshakeTimeoutMicros = 30_000_000

handshake :: Conn -> ConnectOptions -> IO (Session, ServerCaps, Word16)
handshake conn opts = do
  let imax = inboundMaxOf opts
  writePacket conn (buildConnect opts)
  mres <- timeout handshakeTimeoutMicros (loop imax)
  maybe (throwIO (ProtocolViolation ConnectTimedOut)) pure mres
  where
    loop imax = do
      pkt <- readPacket imax conn
      case pkt of
        ConnAck ca
          | isError ca.reasonCode ->
              throwIO (ProtocolViolation (ConnectionRefused ca.reasonCode ca.properties))
          | otherwise ->
              pure
                ( Session
                    { sessionPresent = ca.sessionPresent
                    , reasonCode = ca.reasonCode
                    , assignedClientId = findProperty assignedIdProp ca.properties
                    , serverProperties = ca.properties
                    }
                , serverCaps ca.properties
                , effectiveKeepAlive opts ca.properties
                )
        Auth a
          | a.reasonCode == ContinueAuthentication ->
              case opts.authenticator of
                Nothing -> throwIO (ProtocolViolation (UnexpectedPacket "AUTH without authenticator"))
                Just auth -> do
                  checkAuthMethod auth.method a.properties
                  resp <-
                    auth.step
                      AuthChallenge
                        { reasonCode = a.reasonCode
                        , authData = findProperty authDataProp a.properties
                        , properties = a.properties
                        }
                  let props =
                        AuthenticationMethod auth.method
                          : maybe [] (pure . AuthenticationData) resp.authData
                            <> resp.properties
                  writePacket conn (Auth (AuthPacket ContinueAuthentication props))
                  loop imax
          | otherwise -> throwIO (ProtocolViolation (AuthenticationFailed a.reasonCode))
        _ -> throwIO (ProtocolViolation (UnexpectedPacket "expected CONNACK or AUTH"))

buildConnect :: ConnectOptions -> Packet
buildConnect opts =
  Connect
    ConnectPacket
      { clientId = opts.clientId
      , cleanStart = opts.cleanStart
      , keepAlive = opts.keepAlive
      , username = opts.username
      , password = opts.password
      , will = opts.will
      , properties = userProps <> [ReceiveMaximum (clampRecvMax opts.receiveQueueBound)] <> authProps
      }
  where
    -- We do not support inbound Topic Aliases, so never advertise a Topic Alias
    -- Maximum even if the caller put one in their properties.
    userProps = filter (not . isTopicAliasMaximum) opts.properties
    isTopicAliasMaximum = \case TopicAliasMaximum _ -> True; _ -> False
    authProps = case opts.authenticator of
      Nothing -> []
      Just a -> AuthenticationMethod a.method : maybe [] (pure . AuthenticationData) a.initial

clampRecvMax :: Int -> Word16
clampRecvMax n = fromIntegral (max 1 (min 65535 n))

inboundMaxOf :: ConnectOptions -> MaxPacketSize
inboundMaxOf opts =
  maybe defaultMaxPacketSize fromIntegral (findProperty maxPktProp opts.properties)

effectiveKeepAlive :: ConnectOptions -> Properties -> Word16
effectiveKeepAlive opts props = fromMaybe opts.keepAlive (findProperty serverKaProp props)

serverCaps :: Properties -> ServerCaps
serverCaps props =
  ServerCaps
    { receiveMax = findD 65535 recvMaxProp
    , maximumQoS = findD QoS2 (\case MaximumQoS q -> Just q; _ -> Nothing)
    , retainAvailable = findD True (\case RetainAvailable b -> Just b; _ -> Nothing)
    , maxPacketSize = findProperty maxPktProp props
    , topicAliasMax = findD 0 (\case TopicAliasMaximum v -> Just v; _ -> Nothing)
    , wildcardAvailable = findD True (\case WildcardSubscriptionAvailable b -> Just b; _ -> Nothing)
    , subIdAvailable = findD True (\case SubscriptionIdentifierAvailable b -> Just b; _ -> Nothing)
    , sharedAvailable = findD True (\case SharedSubscriptionAvailable b -> Just b; _ -> Nothing)
    }
  where
    findD d f = fromMaybe d (findProperty f props)

-- Threads -------------------------------------------------------------------

newClientState :: Conn -> ConnectOptions -> ServerCaps -> Word16 -> IO ClientState
newClientState conn opts sc eka = do
  now <- getCurrentTime
  connV <- newTVarIO conn
  capsV <- newTVarIO sc
  wlock <- newMVar ()
  pidV <- newTVarIO (PidPool 1 IntSet.empty)
  pendV <- SM.newIO
  inQ <- newTBQueueIO (fromIntegral (max 1 opts.receiveQueueBound))
  ackQ <- newTQueueIO
  inq2 <- SM.newIO
  aIn <- SM.newIO
  aOut <- newTVarIO (AliasOut 1 Map.empty)
  outF <- SM.newIO
  quota <- newTVarIO (fromIntegral sc.receiveMax)
  ls <- newTVarIO now
  lr <- newTVarIO now
  po <- newTVarIO False
  pw <- newTVarIO []
  ra <- newTVarIO False
  av <- newEmptyTMVarIO
  cv <- newEmptyTMVarIO
  th <- newTVarIO []
  kaV <- newTVarIO eka
  pure
    ClientState
      { connVar = connV
      , caps = capsV
      , writeLock = wlock
      , pidPool = pidV
      , pending = pendV
      , incoming = inQ
      , ackQueue = ackQ
      , inQoS2 = inq2
      , aliasIn = aIn
      , aliasOut = aOut
      , outInflight = outF
      , sendQuota = quota
      , lastSent = ls
      , lastRecv = lr
      , pingOut = po
      , pingWaiters = pw
      , reauthActive = ra
      , authVar = av
      , closedVar = cv
      , threads = th
      , options = opts
      , inboundMax = inboundMaxOf opts
      , keepAliveSecs = kaV
      }

startThreads :: ClientState -> IO ()
startThreads st = do
  rd <- async (supervised st (readerLoop st))
  ka <- async (supervised st (keepaliveLoop st))
  aw <- async (supervised st (ackWriterLoop st))
  atomically (writeTVar st.threads [rd, ka, aw])

stopThreads :: ClientState -> IO ()
stopThreads st = do
  ts <- readTVarIO st.threads
  atomically (writeTVar st.threads [])
  mapM_ cancel ts

supervised :: ClientState -> IO () -> IO ()
supervised st act =
  act
    `catches` [ Handler (\(e :: AsyncCancelled) -> throwIO e)
              , Handler (\(e :: MqttException) -> recordClosed st (ClosedByException e))
              , Handler (\(_ :: SomeException) -> recordClosed st (ClosedByException (TransportClosed ConnectionClosed)))
              ]

readerLoop :: ClientState -> IO ()
readerLoop st = loop
  where
    loop = do
      conn <- readTVarIO st.connVar
      pkt <- readPacket st.inboundMax conn
      now <- getCurrentTime
      atomically (writeTVar st.lastRecv now)
      cont <- dispatch st pkt
      when cont loop

dispatch :: ClientState -> Packet -> IO Bool
dispatch st = \case
  Publish p -> handleInboundPublish st p >> pure True
  PubAck p -> ackOrOrphan st p.packetId (RPubAck p) cleanupOrphan >> pure True
  PubRec p -> do
    done <- atomically (fulfill st p.packetId (RPubRec p))
    unless done (orphanPubRec st p)
    pure True
  PubComp p -> ackOrOrphan st p.packetId (RPubComp p) cleanupOrphan >> pure True
  PubRel p -> handleInboundPubRel st p >> pure True
  SubAck s -> atomically (void (fulfill st s.packetId (RSubAck s))) >> pure True
  UnsubAck s -> atomically (void (fulfill st s.packetId (RUnsubAck s))) >> pure True
  PingResp -> handlePingResp st >> pure True
  Disconnect d -> recordClosed st (ClosedByServer d.reasonCode d.properties) >> pure False
  Auth a -> handleAuth st a >> pure True
  _ -> pure True

{- | Fulfil the waiting request, or — if there is none (e.g. a publish replayed
after reconnect whose original caller has gone) — run the orphan handler so the
QoS handshake still self-completes and the packet id is not leaked.
-}
ackOrOrphan :: ClientState -> PacketId -> PendingResult -> (ClientState -> PacketId -> IO ()) -> IO ()
ackOrOrphan st pid res orphan = do
  done <- atomically (fulfill st pid res)
  unless done (orphan st pid)

-- | Insert a result into the waiter's slot. Returns whether a waiter was present.
fulfill :: ClientState -> PacketId -> PendingResult -> STM Bool
fulfill st (PacketId pid) res = do
  mt <- SM.lookup pid st.pending
  case mt of
    Just tmv -> putTMVar tmv res >> SM.delete pid st.pending >> pure True
    Nothing -> pure False

{- | A QoS 1 PUBACK / QoS 2 PUBCOMP for an orphaned in-flight id: drop the entry
and free the id.
-}
cleanupOrphan :: ClientState -> PacketId -> IO ()
cleanupOrphan st (PacketId pid) = atomically do
  held <- isJust <$> SM.lookup pid st.outInflight
  when held do
    SM.delete pid st.outInflight
    freePid st pid

{- | A QoS 2 PUBREC for an orphaned in-flight id. A success PUBREC advances the
entry and sends PUBREL so a later PUBCOMP can clean it up; an error PUBREC
(@>= 0x80@) ends the exchange — drop the entry and free the id, never PUBREL.
-}
orphanPubRec :: ClientState -> PubAckPacket -> IO ()
orphanPubRec st p
  | isError p.reasonCode = cleanupOrphan st p.packetId
  | otherwise = do
      let PacketId pid = p.packetId
      present <- atomically do
        held <- isJust <$> SM.lookup pid st.outInflight
        if held
          then SM.insert OutQoS2Rel pid st.outInflight >> pure True
          else pure False
      when present (sendPacket st (PubRel (PubAckPacket (PacketId pid) Success [])))

handlePingResp :: ClientState -> IO ()
handlePingResp st = atomically do
  writeTVar st.pingOut False
  ws <- readTVar st.pingWaiters
  mapM_ (\t -> void (tryPutTMVar t ())) ws
  writeTVar st.pingWaiters []

handleAuth :: ClientState -> AuthPacket -> IO ()
handleAuth st a = do
  active <- readTVarIO st.reauthActive
  when active (atomically (void (tryPutTMVar st.authVar a)))

handleInboundPublish :: ClientState -> PublishPacket -> IO ()
handleInboundPublish st p = do
  etop <- resolveAlias st p
  case etop of
    Left err -> throwIO (ProtocolViolation err)
    Right top -> do
      let msg =
            Message
              { topic = top
              , payload = p.payload
              , qos = p.qos
              , retain = p.retain
              , dup = p.dup
              , properties = p.properties
              }
      case (p.qos, p.packetId) of
        (QoS0, _) -> deliverQoS0 st (Inbound msg Nothing Nothing)
        (QoS1, Just (PacketId pid)) ->
          deliver st (Inbound msg (Just (PubAck (PubAckPacket (PacketId pid) Success []))) Nothing)
        (QoS2, Just (PacketId pid)) -> handleQoS2Publish st pid msg
        _ -> throwIO (ProtocolViolation (UnexpectedPacket "PUBLISH QoS without packet id"))

handleQoS2Publish :: ClientState -> Word16 -> Message -> IO ()
handleQoS2Publish st pid msg = do
  atomically do
    existing <- SM.lookup pid st.inQoS2
    case existing of
      Just _ -> pure ()
      Nothing -> SM.insert (AwaitingPubRel msg) pid st.inQoS2
  sendPacket st (PubRec (PubAckPacket (PacketId pid) Success []))

handleInboundPubRel :: ClientState -> PubAckPacket -> IO ()
handleInboundPubRel st p = do
  let PacketId pid = p.packetId
  action <- atomically do
    existing <- SM.lookup pid st.inQoS2
    case existing of
      Just (AwaitingPubRel msg) -> do
        SM.insert AwaitingConsume pid st.inQoS2
        pure (Just msg)
      Just AwaitingConsume -> pure Nothing
      Nothing -> pure Nothing
  case action of
    Just msg ->
      deliver st (Inbound msg (Just (PubComp (PubAckPacket (PacketId pid) Success []))) (Just pid))
    Nothing ->
      -- Unknown id, or duplicate PUBREL while awaiting consume. For an id with no
      -- record at all, answer PUBCOMP 0x92; an AwaitingConsume duplicate is left
      -- for the deferred PUBCOMP on consume.
      whenM (not <$> hasInQoS2 st pid) $
        sendPacket st (PubComp (PubAckPacket (PacketId pid) PacketIdentifierNotFound []))

hasInQoS2 :: ClientState -> Word16 -> IO Bool
hasInQoS2 st pid = atomically (isJust <$> SM.lookup pid st.inQoS2)

deliver :: ClientState -> Inbound -> IO ()
deliver st inb = atomically (writeTBQueue st.incoming inb)

deliverQoS0 :: ClientState -> Inbound -> IO ()
deliverQoS0 st inb = atomically case st.options.overflowPolicy of
  Block -> writeTBQueue st.incoming inb
  DropNewest -> do
    full <- isFullTBQueue st.incoming
    unless full (writeTBQueue st.incoming inb)
  DropOldest -> do
    full <- isFullTBQueue st.incoming
    when full (void (tryReadTBQueue st.incoming))
    writeTBQueue st.incoming inb

resolveAlias :: ClientState -> PublishPacket -> IO (Either ProtocolError Topic)
resolveAlias _st p =
  case findProperty topicAliasProp p.properties of
    Nothing
      | T.null p.topic -> pure (Left (UnexpectedPacket "empty topic without alias"))
      | otherwise -> pure (Right (Topic p.topic))
    Just _ ->
      -- We never advertise a Topic Alias Maximum, so the server must not send one.
      pure (Left (UnexpectedPacket "unexpected topic alias"))

ackWriterLoop :: ClientState -> IO ()
ackWriterLoop st = forever do
  pkt <- atomically (readTQueue st.ackQueue)
  sendPacket st pkt

keepaliveLoop :: ClientState -> IO ()
keepaliveLoop st = do
  ka <- readTVarIO st.keepAliveSecs
  when (ka > 0) (loop ka)
  where
    loop ka = do
      threadDelay (fromIntegral ka * 500_000)
      now <- getCurrentTime
      ls <- readTVarIO st.lastSent
      lr <- readTVarIO st.lastRecv
      po <- readTVarIO st.pingOut
      if po && secondsSince now lr > fromIntegral ka * 1.5
        then throwIO (ProtocolViolation KeepAliveExpired)
        else do
          when (secondsSince now ls >= fromIntegral ka) do
            atomically (writeTVar st.pingOut True)
            sendPacket st PingReq
          loop ka

secondsSince :: UTCTime -> UTCTime -> Double
secondsSince now t = realToFrac (diffUTCTime now t)

-- Sending -------------------------------------------------------------------

sendPacket :: ClientState -> Packet -> IO ()
sendPacket st pkt =
  withMVar st.writeLock \_ -> do
    conn <- readTVarIO st.connVar
    sc <- readTVarIO st.caps
    writeChecked conn sc pkt
    now <- getCurrentTime
    atomically (writeTVar st.lastSent now)

{- | Encode a packet and write it, refusing to exceed the server's negotiated
Maximum Packet Size (§3.2.2.3.4).
-}
writeChecked :: Conn -> ServerCaps -> Packet -> IO ()
writeChecked conn sc pkt = do
  let bytes = encodePacketBS pkt
      size = BS.length bytes
  case sc.maxPacketSize of
    Just lim | fromIntegral size > lim -> throwIO (ProtocolViolation (OversizePacket size))
    _ -> conn.base.connectionWrite bytes

{- | Hold one outbound QoS>0 send-quota slot for the action, bounding concurrent
unacknowledged QoS 1\/2 publishes to the server's Receive Maximum. Does not
block once the connection is closed.
-}
withQuota :: ClientState -> IO a -> IO a
withQuota st = bracket_ acquire release
  where
    acquire = atomically do
      closed <- tryReadTMVar st.closedVar
      n <- readTVar st.sendQuota
      case closed of
        Just _ -> writeTVar st.sendQuota (n - 1)
        Nothing -> if n <= 0 then retry else writeTVar st.sendQuota (n - 1)
    -- Clamp on release so a slot released after a reconnect reset cannot push the
    -- quota above the (possibly new) server Receive Maximum.
    release = atomically do
      sc <- readTVar st.caps
      modifyTVar' st.sendQuota (min (fromIntegral sc.receiveMax) . (+ 1))

{- | Send a PUBLISH, applying outbound Topic Alias atomically inside the write
lock so two concurrent publishes to the same topic cannot reorder the
establishing packet after an alias-only one.
-}
sendPublish :: ClientState -> Topic -> PublishPacket -> IO ()
sendPublish st top basePkt =
  withMVar st.writeLock \_ -> do
    conn <- readTVarIO st.connVar
    sc <- readTVarIO st.caps
    pkt <- case st.options.topicAliasSending of
      AliasUpTo cap | sc.topicAliasMax > 0 -> applyAlias st top basePkt (min cap sc.topicAliasMax)
      _ -> pure basePkt
    writeChecked conn sc (Publish pkt)
    now <- getCurrentTime
    atomically (writeTVar st.lastSent now)

applyAlias :: ClientState -> Topic -> PublishPacket -> Word16 -> IO PublishPacket
applyAlias st top basePkt limit = do
  AliasOut nextA m <- readTVarIO st.aliasOut
  case Map.lookup top.raw m of
    Just a -> pure (withAlias "" a)
    Nothing
      | nextA <= limit -> do
          atomically (writeTVar st.aliasOut (AliasOut (nextA + 1) (Map.insert top.raw nextA m)))
          pure (withAlias basePkt.topic nextA)
      | otherwise -> pure basePkt
  where
    -- Reconstruct rather than record-update: the shared field names make a record
    -- update ambiguous under DuplicateRecordFields.
    withAlias t a =
      PublishPacket
        { topic = t
        , packetId = basePkt.packetId
        , qos = basePkt.qos
        , retain = basePkt.retain
        , dup = basePkt.dup
        , payload = basePkt.payload
        , properties = TopicAlias a : basePkt.properties
        }

-- Helpers -------------------------------------------------------------------

basePublish :: Topic -> ByteString -> PublishOptions -> Maybe PacketId -> PublishPacket
basePublish top body opts mpid =
  PublishPacket
    { topic = top.raw
    , packetId = mpid
    , qos = opts.qos
    , retain = opts.retain
    , dup = False
    , payload = body
    , properties = opts.properties
    }

{- | Allocate a packet id and register a waiter, run an action, and always free
the id and clear the in-flight\/pending entries afterwards.
-}
withRequest :: ClientState -> (Word16 -> TMVar PendingResult -> IO a) -> IO a
withRequest st act = mask \restore -> do
  (pid, tmv) <- atomically do
    p <- allocPid st
    t <- newEmptyTMVar
    SM.insert t p st.pending
    pure (p, t)
  restore (act pid tmv) `finally` atomically do
    SM.delete pid st.pending
    -- Free the id only if it is not held by surviving in-flight state
    -- (a QoS 1\/2 publish awaiting replay keeps its id reserved).
    held <- isJust <$> SM.lookup pid st.outInflight
    unless held (freePid st pid)

{- | Await a request's result, also returning 'RConnectionLost' if the connection
closes — even for a waiter registered after the close notification fired (so
there is no lost-wakeup window).
-}
awaitResult :: ClientState -> TMVar PendingResult -> IO PendingResult
awaitResult st tmv =
  atomically (takeTMVar tmv `orElse` (RConnectionLost <$ readTMVar st.closedVar))

allocPid :: ClientState -> STM Word16
allocPid st = do
  PidPool nxt used <- readTVar st.pidPool
  let wrap x = if x == 0 then 1 else x
      pick cand tries
        | tries > (65535 :: Int) = Nothing
        | IntSet.member (fromIntegral cand) used = pick (wrap (cand + 1)) (tries + 1)
        | otherwise = Just cand
  case pick (wrap nxt) (0 :: Int) of
    Nothing -> retry
    Just chosen -> do
      writeTVar st.pidPool (PidPool (wrap (chosen + 1)) (IntSet.insert (fromIntegral chosen) used))
      pure chosen

freePid :: ClientState -> Word16 -> STM ()
freePid st pid =
  modifyTVar' st.pidPool (\(PidPool n u) -> PidPool n (IntSet.delete (fromIntegral pid) u))

recordClosed :: ClientState -> CloseReason -> IO ()
recordClosed st reason = do
  first <- atomically do
    ok <- tryPutTMVar st.closedVar reason
    when ok do
      waiters <- ListT.toList (SM.listT st.pending)
      mapM_ (\(_, t) -> void (tryPutTMVar t RConnectionLost)) waiters
      SM.reset st.pending
      ws <- readTVar st.pingWaiters
      mapM_ (\t -> void (tryPutTMVar t ())) ws
      writeTVar st.pingWaiters []
    pure ok
  when first do
    conn <- readTVarIO st.connVar
    conn.base.connectionClose `catch` ignoreAll

checkOpen :: ClientState -> IO ()
checkOpen st = do
  closed <- atomically (tryReadTMVar st.closedVar)
  case closed of
    Just (ClosedByException e) -> throwIO e
    Just _ -> throwIO (TransportClosed ConnectionClosed)
    Nothing -> pure ()

checkSubscriptionCap :: ServerCaps -> Subscription -> IO ()
checkSubscriptionCap sc sub = do
  let raw = sub.topicFilter.raw
  when (not sc.wildcardAvailable && T.any (\c -> c == '+' || c == '#') raw) $
    throwIO (ProtocolViolation (UnsupportedByServer "wildcard subscriptions not available"))
  when (not sc.sharedAvailable && "$share/" `T.isPrefixOf` raw) $
    throwIO (ProtocolViolation (UnsupportedByServer "shared subscriptions not available"))

isSubscriptionIdentifier :: Property -> Bool
isSubscriptionIdentifier = \case
  SubscriptionIdentifier _ -> True
  _ -> False

ignoreAll :: SomeException -> IO ()
ignoreAll _ = pure ()

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb act = mb >>= \b -> when b act

-- Property projections ------------------------------------------------------

recvMaxProp :: Property -> Maybe Word16
recvMaxProp = \case ReceiveMaximum v -> Just v; _ -> Nothing

maxPktProp :: Property -> Maybe Word32
maxPktProp = \case MaximumPacketSize v -> Just v; _ -> Nothing

topicAliasProp :: Property -> Maybe Word16
topicAliasProp = \case TopicAlias v -> Just v; _ -> Nothing

serverKaProp :: Property -> Maybe Word16
serverKaProp = \case ServerKeepAlive v -> Just v; _ -> Nothing

assignedIdProp :: Property -> Maybe Text
assignedIdProp = \case AssignedClientIdentifier t -> Just t; _ -> Nothing

authDataProp :: Property -> Maybe ByteString
authDataProp = \case AuthenticationData d -> Just d; _ -> Nothing

{- | Reject an AUTH whose Authentication Method (if present) differs from the one
negotiated at CONNECT (MQTT-4.12.0-x).
-}
checkAuthMethod :: Text -> Properties -> IO ()
checkAuthMethod expected props =
  case findProperty (\case AuthenticationMethod m -> Just m; _ -> Nothing) props of
    Just m | m /= expected -> throwIO (ProtocolViolation (UnexpectedPacket "authentication method mismatch"))
    _ -> pure ()
