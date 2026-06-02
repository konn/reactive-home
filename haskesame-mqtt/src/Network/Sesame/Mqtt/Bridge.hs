module Network.Sesame.Mqtt.Bridge (
  runBridge,
  publishStatus,
  commandFilter,
  statusTopic,
  parseCommandTopic,
  parseCommandPayload,
  parseCommandMessage,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, forConcurrently_, race, race_, waitCatch, withAsync)
import Control.Concurrent.STM (STM, TQueue, TVar, atomically, check, modifyTVar', newTQueueIO, newTVarIO, orElse, readTQueue, readTVar, readTVarIO, registerDelay, writeTQueue, writeTVar)
import Control.Exception (SomeException)
import Control.Exception.Safe qualified as Exception
import Control.Monad (forever, when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.Mqtt.Client.AutoReconnect qualified as Mqtt
import Network.Sesame.Client qualified as Sesame
import Network.Sesame.Codec (decodeSesame5MechStatus)
import Network.Sesame.Exception (SesameException (SesameTransportException), SesameTransportError (TransportClosed))
import Network.Sesame.Mqtt.Types
import Network.Sesame.Types (ItemCode (MechStatus), Sesame5MechStatus (..), SesamePublish (..))
import StmContainers.Map qualified as STMMap
import System.IO (hPutStrLn, stderr)
import System.Random (randomRIO)

runBridge :: Mqtt.AutoClient -> BridgeConfig -> [BridgeDevice] -> IO ()
runBridge mqtt config devices = do
  filter_ <- either (fail . show) pure (commandFilter config)
  debug config ("subscribing command topic filter: " <> show filter_)
  _ <- Mqtt.subscribe1 mqtt filter_ Mqtt.QoS1
  deviceMap <- STMMap.newIO
  statusVersions <- newTVarIO Map.empty
  statusSnapshots <- newTVarIO Map.empty
  lastActivity <- newTVarIO Map.empty
  pendingCommands <- newTVarIO Map.empty
  sessions <- traverse newDeviceSession devices
  atomically (mapM_ (\session -> STMMap.insert session (deviceKey session.sessionDevice.deviceUuid) deviceMap) sessions)
  debug config ("starting bridge for " <> show (length devices) <> " Sesame device(s)")
  race_
    (consumeCommands mqtt config deviceMap pendingCommands)
    (forConcurrently_ sessions (runDeviceSession mqtt config statusVersions statusSnapshots lastActivity pendingCommands))

type DeviceMap = STMMap.Map Text DeviceSession

type StatusVersions = TVar (Map Text Int)

type StatusSnapshots = TVar (Map Text StatusPayload)

type LastActivity = TVar (Map Text UTCTime)

type PendingCommands = TVar (Map Text PendingCommand)

data DeviceSession = DeviceSession
  { sessionDevice :: !BridgeDevice
  , sessionCommandSignal :: !(TQueue ())
  , sessionConnected :: !(TVar Bool)
  }

newDeviceSession :: BridgeDevice -> IO DeviceSession
newDeviceSession device =
  DeviceSession device <$> newTQueueIO <*> newTVarIO False

data PendingCommand = PendingCommand
  { pendingLockCommand :: !LockCommand
  , pendingForceSend :: !Bool
  }

newPendingCommand :: LockCommand -> Bool -> PendingCommand
newPendingCommand command forceSend =
  PendingCommand
    { pendingLockCommand = command
    , pendingForceSend = forceSend
    }

data ReconnectWait = ReconnectWaitElapsed | ReconnectWaitInterruptedByCommand

data ConnectedResult
  = ConnectedByStatusLoop !(Either SomeException ())
  | ConnectedByCommandLoop !CommandLoopResult

data CommandLoopResult
  = CommandLoopReconnect !ReconnectReason
  | CommandLoopConnectionClosed !SomeException

data ReconnectReason
  = ReconnectPostCommandTimeout
  | ReconnectCommandFailure !SomeException
  | ReconnectIdleBeforeCommand

runDeviceSession :: Mqtt.AutoClient -> BridgeConfig -> StatusVersions -> StatusSnapshots -> LastActivity -> PendingCommands -> DeviceSession -> IO ()
runDeviceSession mqtt config statusVersions statusSnapshots lastActivity pendingCommands session =
  go minReconnectDelayMicros
  where
    uuid = session.sessionDevice.deviceUuid
    go reconnectDelayCapMicros = do
      debug config ("connecting Sesame device " <> UUID.toString uuid)
      connectedResult <- Exception.tryAny session.sessionDevice.connectSesameClient
      case connectedResult of
        Left err -> do
          debug config ("Sesame connection failed for " <> UUID.toString uuid <> ": " <> show err)
          nextDelayCapMicros <- waitReconnectDelayAfterFailure config uuid reconnectDelayCapMicros err
          go (nextReconnectDelayMicros nextDelayCapMicros)
        Right connected -> do
          debug config ("Sesame connected " <> UUID.toString uuid)
          atomically (writeTVar session.sessionConnected True)
          recordActivity lastActivity uuid
          result <-
            Exception.tryAny
              ( runConnectedSession mqtt config statusVersions statusSnapshots lastActivity pendingCommands session connected
                  `Exception.finally` do
                    atomically (writeTVar session.sessionConnected False)
                    disconnectConnected config uuid connected
              )
          debug config ("Sesame disconnected " <> UUID.toString uuid <> ": " <> either show describeConnectedResult result)
          _ <- waitBeforeReconnect config pendingCommands uuid
          go minReconnectDelayMicros

runConnectedSession :: Mqtt.AutoClient -> BridgeConfig -> StatusVersions -> StatusSnapshots -> LastActivity -> PendingCommands -> DeviceSession -> ConnectedBridgeDevice -> IO ConnectedResult
runConnectedSession mqtt config statusVersions statusSnapshots lastActivity pendingCommands session connected = do
  inFlightCommand <- newTVarIO Nothing
  settleQueuedCommandAfterLogin config pendingCommands uuid
  withAsync (publishDeviceStatus mqtt config statusVersions statusSnapshots lastActivity uuid connected.sesameClient) \statusAsync ->
    race
      (waitCatch statusAsync)
      (ConnectedByCommandLoop <$> runCommandLoop config statusVersions statusSnapshots lastActivity pendingCommands inFlightCommand session connected)
      >>= \case
        Left statusResult -> do
          requeueInFlightAfterStatusEnd config pendingCommands uuid inFlightCommand statusResult
          pure (ConnectedByStatusLoop statusResult)
        Right commandResult -> pure commandResult
  where
    uuid = session.sessionDevice.deviceUuid

describeConnectedResult :: ConnectedResult -> String
describeConnectedResult = \case
  ConnectedByStatusLoop (Left err) -> show err
  ConnectedByStatusLoop (Right ()) -> "status loop ended"
  ConnectedByCommandLoop (CommandLoopReconnect ReconnectPostCommandTimeout) -> "command requested reconnect after post-command status timeout"
  ConnectedByCommandLoop (CommandLoopReconnect (ReconnectCommandFailure err)) -> "command requested reconnect after failure: " <> show err
  ConnectedByCommandLoop (CommandLoopReconnect ReconnectIdleBeforeCommand) -> "command requested reconnect before sending on idle Sesame session"
  ConnectedByCommandLoop (CommandLoopConnectionClosed err) -> show err

disconnectConnected :: BridgeConfig -> UUID -> ConnectedBridgeDevice -> IO ()
disconnectConnected config uuid connected = do
  debug config ("closing Sesame session " <> UUID.toString uuid)
  _ <- Exception.tryAny connected.disconnectSesameClient
  pure ()

publishDeviceStatus :: Mqtt.AutoClient -> BridgeConfig -> StatusVersions -> StatusSnapshots -> LastActivity -> UUID -> Sesame.Sesame5Client -> IO ()
publishDeviceStatus mqtt config statusVersions statusSnapshots lastActivity uuid sesame =
  forever do
    SesamePublish publishItem payload <- Sesame.readPublish sesame
    recordActivity lastActivity uuid
    debug config ("received Sesame publish from " <> UUID.toString uuid <> ": " <> show publishItem)
    case publishItem of
      MechStatus ->
        case decodeSesame5MechStatus payload of
          Left err -> debug config ("failed to decode mech status from " <> UUID.toString uuid <> ": " <> show err)
          Right status -> do
            debug config ("publishing status for " <> UUID.toString uuid <> ": " <> describeMechStatus status)
            let statusPayload = statusFromMech status
            recordStatus statusVersions statusSnapshots uuid statusPayload
            publishStatus mqtt config uuid statusPayload
      _ -> pure ()

consumeCommands :: Mqtt.AutoClient -> BridgeConfig -> DeviceMap -> PendingCommands -> IO ()
consumeCommands mqtt config devices pendingCommands =
  forever do
    message <- Mqtt.recvMessage mqtt
    case parseCommandMessage config message of
      Left err -> debug config ("ignoring MQTT command: " <> show err)
      Right (uuid, command) -> do
        debug config ("received MQTT command for " <> UUID.toString uuid <> ": command=" <> show command <> ", qos=" <> show message.qos <> ", retain=" <> show message.retain <> ", dup=" <> show message.dup)
        lookupDeviceSession devices uuid >>= \case
          Nothing -> debug config ("ignoring command for unknown Sesame device: " <> UUID.toString uuid)
          Just (session, connected) ->
            if connected || shouldKeepDisconnectedCommand message.qos
              then do
                queuePendingCommand config pendingCommands session uuid command False ("QoS " <> show message.qos <> " command")
              else debug config ("ignoring QoS0 command while Sesame device may be unavailable: " <> UUID.toString uuid)

lookupDeviceSession :: DeviceMap -> UUID -> IO (Maybe (DeviceSession, Bool))
lookupDeviceSession devices uuid =
  atomically do
    STMMap.lookup (deviceKey uuid) devices >>= \case
      Nothing -> pure Nothing
      Just session -> do
        connected <- readTVar session.sessionConnected
        pure (Just (session, connected))

runCommandLoop :: BridgeConfig -> StatusVersions -> StatusSnapshots -> LastActivity -> PendingCommands -> TVar (Maybe PendingCommand) -> DeviceSession -> ConnectedBridgeDevice -> IO CommandLoopResult
runCommandLoop config statusVersions statusSnapshots lastActivity pendingCommands inFlightCommand session connected =
  takePendingCommandBlocking pendingCommands session >>= drain
  where
    uuid = session.sessionDevice.deviceUuid
    drain nextCommand = do
      atomically (writeTVar inFlightCommand (Just nextCommand))
      result <- executeCommandUnlocked config statusVersions statusSnapshots lastActivity pendingCommands uuid connected nextCommand
      atomically (writeTVar inFlightCommand Nothing)
      case result of
        CommandDone ->
          takePendingCommand pendingCommands uuid >>= \case
            Nothing -> takePendingCommandBlocking pendingCommands session >>= drain
            Just pendingCommand -> do
              debug config ("running pending command for " <> UUID.toString uuid <> ": command=" <> show pendingCommand.pendingLockCommand <> ", force_send=" <> show pendingCommand.pendingForceSend)
              drain pendingCommand
        CommandNeedsReconnect reason -> pure (CommandLoopReconnect reason)
        CommandConnectionClosed err -> pure (CommandLoopConnectionClosed err)

data CommandExecutionResult
  = CommandDone
  | CommandNeedsReconnect !ReconnectReason
  | CommandConnectionClosed !SomeException

executeCommandUnlocked :: BridgeConfig -> StatusVersions -> StatusSnapshots -> LastActivity -> PendingCommands -> UUID -> ConnectedBridgeDevice -> PendingCommand -> IO CommandExecutionResult
executeCommandUnlocked config statusVersions statusSnapshots lastActivity pendingCommands uuid connected command =
  commandIdleAge lastActivity uuid >>= \case
    Just idleAge | idleAge >= commandIdleRefreshAge -> do
      requeued <- requeueFailedCommand config pendingCommands uuid command.pendingLockCommand ("idle Sesame session refresh before command after " <> show (round (realToFrac idleAge :: Double) :: Int) <> "s idle")
      if requeued
        then pure (CommandNeedsReconnect ReconnectIdleBeforeCommand)
        else pure CommandDone
    _ ->
      readStatusSnapshot statusSnapshots uuid >>= \case
        Just status
          | not command.pendingForceSend && commandSatisfied command.pendingLockCommand status ->
              debug config ("skipping command already satisfied for " <> UUID.toString uuid <> ": command=" <> show command.pendingLockCommand <> ", status=" <> show status) *> pure CommandDone
        _ -> do
          beforeStatusVersion <- readStatusVersion statusVersions uuid
          result <- Exception.tryAny (sendCommandAndWaitForStatus config statusVersions statusSnapshots uuid connected command.pendingLockCommand beforeStatusVersion)
          case result of
            Right True -> pure CommandDone
            Right False -> do
              requeued <- requeueFailedCommand config pendingCommands uuid command.pendingLockCommand "post-command status timeout"
              if requeued
                then pure (CommandNeedsReconnect ReconnectPostCommandTimeout)
                else pure CommandDone
            Left err -> do
              debug config ("command failed for " <> UUID.toString uuid <> ": " <> show err)
              _ <- requeueFailedCommand config pendingCommands uuid command.pendingLockCommand "command failure"
              if isTransportClosedException err
                then pure (CommandConnectionClosed err)
                else pure (CommandNeedsReconnect (ReconnectCommandFailure err))

sendCommandAndWaitForStatus :: BridgeConfig -> StatusVersions -> StatusSnapshots -> UUID -> ConnectedBridgeDevice -> LockCommand -> Int -> IO Bool
sendCommandAndWaitForStatus config statusVersions statusSnapshots uuid connected command beforeStatusVersion = do
  let sendAction = case command of
        CommandLock -> debug config ("locking " <> UUID.toString uuid) *> Sesame.lock connected.sesameClient config.historyName
        CommandUnlock -> debug config ("unlocking " <> UUID.toString uuid) *> Sesame.unlock connected.sesameClient config.historyName
      statusAction = waitForCommandStatus statusVersions statusSnapshots uuid command beforeStatusVersion commandStatusWaitMicros
  withAsync sendAction \sendAsync ->
    race (waitCatch sendAsync) statusAction >>= \case
      Left (Right ()) -> waitAfterCommand config statusVersions statusSnapshots uuid command beforeStatusVersion
      Left (Left err) -> Exception.throwIO err
      Right True -> do
        debug config ("post-command Sesame status satisfied before command response for " <> UUID.toString uuid <> ": command=" <> show command)
        threadDelay commandSettleMicros
        pure True
      Right False -> do
        debug config ("timed out waiting for post-command Sesame status before command response from " <> UUID.toString uuid <> ": command=" <> show command <> "; waiting for late command response/status")
        waitForLateCommandResponseOrStatus config statusVersions statusSnapshots uuid command beforeStatusVersion sendAsync

waitForLateCommandResponseOrStatus :: BridgeConfig -> StatusVersions -> StatusSnapshots -> UUID -> LockCommand -> Int -> Async () -> IO Bool
waitForLateCommandResponseOrStatus config statusVersions statusSnapshots uuid command beforeStatusVersion sendAsync =
  race (waitCatch sendAsync) statusAction >>= \case
    Left (Right ()) -> waitAfterCommandWithTimeout config statusVersions statusSnapshots uuid command beforeStatusVersion commandResponseGraceMicros
    Left (Left err) -> Exception.throwIO err
    Right True -> do
      debug config ("post-command Sesame status satisfied during late command response grace for " <> UUID.toString uuid <> ": command=" <> show command)
      threadDelay commandSettleMicros
      pure True
    Right False -> do
      debug config ("timed out waiting for late command response/status from " <> UUID.toString uuid <> ": command=" <> show command)
      pure False
  where
    statusAction = waitForCommandStatus statusVersions statusSnapshots uuid command beforeStatusVersion commandResponseGraceMicros

settleQueuedCommandAfterLogin :: BridgeConfig -> PendingCommands -> UUID -> IO ()
settleQueuedCommandAfterLogin config pendingCommands uuid = do
  pendingAtConnect <- hasPendingCommand pendingCommands uuid
  when pendingAtConnect do
    debug config ("queued command pending after Sesame login; settling before command for " <> UUID.toString uuid <> ": delay_us=" <> show queuedCommandSettleMicros)
    threadDelay queuedCommandSettleMicros

hasPendingCommand :: PendingCommands -> UUID -> IO Bool
hasPendingCommand pendingCommands uuid =
  Map.member (deviceKey uuid) <$> readTVarIO pendingCommands

queuePendingCommand :: BridgeConfig -> PendingCommands -> DeviceSession -> UUID -> LockCommand -> Bool -> String -> IO ()
queuePendingCommand config pendingCommands session uuid command forceSend reason = do
  atomically do
    modifyTVar' pendingCommands (Map.insert (deviceKey uuid) (newPendingCommand command forceSend))
    writeTQueue session.sessionCommandSignal ()
  debug config ("queued latest command because " <> reason <> " for " <> UUID.toString uuid <> ": command=" <> show command <> ", force_send=" <> show forceSend)

takePendingCommand :: PendingCommands -> UUID -> IO (Maybe PendingCommand)
takePendingCommand pendingCommands uuid =
  atomically do
    let key = deviceKey uuid
    command <- Map.lookup key <$> readTVar pendingCommands
    modifyTVar' pendingCommands (Map.delete key)
    pure command

takePendingCommandBlocking :: PendingCommands -> DeviceSession -> IO PendingCommand
takePendingCommandBlocking pendingCommands session =
  atomically loop
  where
    uuid = session.sessionDevice.deviceUuid
    key = deviceKey uuid
    loop = do
      command <- Map.lookup key <$> readTVar pendingCommands
      case command of
        Just pendingCommand -> do
          modifyTVar' pendingCommands (Map.delete key)
          pure pendingCommand
        Nothing -> readTQueue session.sessionCommandSignal *> loop

requeueFailedCommand :: BridgeConfig -> PendingCommands -> UUID -> LockCommand -> String -> IO Bool
requeueFailedCommand config pendingCommands uuid command reason = do
  requeued <-
    atomically do
      let key = deviceKey uuid
      pending <- Map.member key <$> readTVar pendingCommands
      requeued <-
        if pending
          then pure False
          else modifyTVar' pendingCommands (Map.insert key (newPendingCommand command False)) *> pure True
      pure requeued
  if requeued
    then debug config ("requeued command after " <> reason <> " for " <> UUID.toString uuid <> ": command=" <> show command <> ", force_send=False")
    else debug config ("left newer pending command after " <> reason <> " for " <> UUID.toString uuid <> ": failed_command=" <> show command)
  pure requeued

requeueInFlightAfterStatusEnd :: BridgeConfig -> PendingCommands -> UUID -> TVar (Maybe PendingCommand) -> Either SomeException () -> IO ()
requeueInFlightAfterStatusEnd config pendingCommands uuid inFlightCommand = \case
  Left err -> do
    inFlight <- readTVarIO inFlightCommand
    case inFlight of
      Nothing -> pure ()
      Just command -> do
        _ <- requeueFailedCommand config pendingCommands uuid command.pendingLockCommand ("connection closed while command was in flight: " <> show err)
        pure ()
  Right () -> pure ()

isTransportClosedException :: SomeException -> Bool
isTransportClosedException err =
  case Exception.fromException err of
    Just (SesameTransportException TransportClosed) -> True
    _ -> False

publishStatus :: Mqtt.AutoClient -> BridgeConfig -> UUID -> StatusPayload -> IO ()
publishStatus mqtt config uuid status = do
  topic <- either (fail . show) pure (statusTopic config uuid)
  let opts = Mqtt.PublishOptions Mqtt.QoS1 True []
  debug config ("publishing retained status to " <> show topic)
  _ <- Mqtt.publish mqtt topic (LBS.toStrict (encodeStatusPayload status)) opts
  pure ()

commandFilter :: BridgeConfig -> Either BridgeError Mqtt.TopicFilter
commandFilter config
  | "/" `T.isInfixOf` config.baseTopic = Left (InvalidBaseTopic config.baseTopic)
  | otherwise = mapTopicError (Mqtt.mkTopicFilter (config.baseTopic <> "/+/set"))

statusTopic :: BridgeConfig -> UUID -> Either BridgeError Mqtt.Topic
statusTopic config uuid
  | "/" `T.isInfixOf` config.baseTopic = Left (InvalidBaseTopic config.baseTopic)
  | otherwise = mapTopicError (Mqtt.mkTopic (config.baseTopic <> "/" <> T.pack (UUID.toString uuid) <> "/get"))

parseCommandMessage :: BridgeConfig -> Mqtt.Message -> Either BridgeError (UUID, LockCommand)
parseCommandMessage config message =
  (,)
    <$> parseCommandTopic config message.topic
    <*> parseCommandPayload message.payload

parseCommandTopic :: BridgeConfig -> Mqtt.Topic -> Either BridgeError UUID
parseCommandTopic config (Mqtt.Topic topicText) =
  case T.splitOn "/" topicText of
    [base, uuidText, "set"]
      | base == config.baseTopic ->
          maybe (Left (InvalidTopic topicText)) Right (UUID.fromString (T.unpack uuidText))
    _ -> Left (InvalidTopic topicText)

parseCommandPayload :: ByteString -> Either BridgeError LockCommand
parseCommandPayload payload =
  case TE.decodeUtf8' payload of
    Left _ -> Left (InvalidCommandPayload "<non-utf8>")
    Right "LOCKED" -> Right CommandLock
    Right "UNLOCKED" -> Right CommandUnlock
    Right other -> Left (InvalidCommandPayload other)

mapTopicError :: Either Mqtt.TopicError a -> Either BridgeError a
mapTopicError = either (Left . InvalidTopic . T.pack . show) Right

recordStatus :: StatusVersions -> StatusSnapshots -> UUID -> StatusPayload -> IO ()
recordStatus statusVersions statusSnapshots uuid statusPayload =
  atomically do
    modifyTVar' statusVersions (Map.insertWith (+) key 1)
    modifyTVar' statusSnapshots (Map.insert key statusPayload)
  where
    key = deviceKey uuid

recordActivity :: LastActivity -> UUID -> IO ()
recordActivity lastActivity uuid = do
  now <- getCurrentTime
  atomically (modifyTVar' lastActivity (Map.insert (deviceKey uuid) now))

commandIdleAge :: LastActivity -> UUID -> IO (Maybe NominalDiffTime)
commandIdleAge lastActivity uuid = do
  now <- getCurrentTime
  fmap (diffUTCTime now) . Map.lookup (deviceKey uuid) <$> readTVarIO lastActivity

readStatusVersion :: StatusVersions -> UUID -> IO Int
readStatusVersion statusVersions uuid =
  Map.findWithDefault 0 (deviceKey uuid) <$> readTVarIO statusVersions

readStatusSnapshot :: StatusSnapshots -> UUID -> IO (Maybe StatusPayload)
readStatusSnapshot statusSnapshots uuid =
  Map.lookup (deviceKey uuid) <$> readTVarIO statusSnapshots

waitAfterCommand :: BridgeConfig -> StatusVersions -> StatusSnapshots -> UUID -> LockCommand -> Int -> IO Bool
waitAfterCommand config statusVersions statusSnapshots uuid command beforeStatusVersion =
  waitAfterCommandWithTimeout config statusVersions statusSnapshots uuid command beforeStatusVersion commandStatusWaitMicros

waitAfterCommandWithTimeout :: BridgeConfig -> StatusVersions -> StatusSnapshots -> UUID -> LockCommand -> Int -> Int -> IO Bool
waitAfterCommandWithTimeout config statusVersions statusSnapshots uuid command beforeStatusVersion timeoutMicros = do
  debug config ("waiting for post-command Sesame status from " <> UUID.toString uuid <> ": command=" <> show command)
  statusSatisfied <- waitForCommandStatus statusVersions statusSnapshots uuid command beforeStatusVersion timeoutMicros
  if statusSatisfied
    then debug config ("post-command Sesame status satisfied for " <> UUID.toString uuid <> ": command=" <> show command)
    else debug config ("timed out waiting for post-command Sesame status from " <> UUID.toString uuid <> ": command=" <> show command)
  threadDelay commandSettleMicros
  pure statusSatisfied

shouldKeepDisconnectedCommand :: Mqtt.QoS -> Bool
shouldKeepDisconnectedCommand Mqtt.QoS0 = False
shouldKeepDisconnectedCommand Mqtt.QoS1 = True
shouldKeepDisconnectedCommand Mqtt.QoS2 = True

commandSatisfied :: LockCommand -> StatusPayload -> Bool
commandSatisfied CommandLock status = status.lockCurrentState == Locked
commandSatisfied CommandUnlock status = status.lockCurrentState == Unlocked

describeMechStatus :: Sesame5MechStatus -> String
describeMechStatus status =
  show (statusFromMech status)
    <> ", target="
    <> show status.target
    <> ", position="
    <> show status.position
    <> ", flags=0x"
    <> showHex2 status.statusFlags

showHex2 :: (Integral a) => a -> String
showHex2 value =
  let digits = "0123456789abcdef"
      n = fromIntegral value
   in [digits !! (n `div` 16), digits !! (n `mod` 16)]

waitForCommandStatus :: StatusVersions -> StatusSnapshots -> UUID -> LockCommand -> Int -> Int -> IO Bool
waitForCommandStatus statusVersions statusSnapshots uuid command beforeStatusVersion timeoutMicros = do
  timedOut <- registerDelay timeoutMicros
  ( do
      versions <- readTVar statusVersions
      snapshots <- readTVar statusSnapshots
      let key = deviceKey uuid
      check (Map.findWithDefault 0 key versions > beforeStatusVersion)
      check (maybe False (commandSatisfied command) (Map.lookup key snapshots))
      pure True
    )
    `orTimeout` timedOut

waitBeforeReconnect :: BridgeConfig -> PendingCommands -> UUID -> IO ReconnectWait
waitBeforeReconnect config pendingCommands uuid = do
  pendingNow <- hasPendingCommand pendingCommands uuid
  if pendingNow
    then waitForCommandReconnectSettle config uuid
    else do
      debug config ("waiting before passive Sesame reconnect for " <> UUID.toString uuid <> ": delay_us=" <> show passiveReconnectSettleMicros)
      commandArrived <- waitForPendingCommandOrDelay pendingCommands uuid passiveReconnectSettleMicros
      if commandArrived
        then waitForCommandReconnectSettle config uuid
        else debug config ("passively reconnecting Sesame device after disconnect " <> UUID.toString uuid) *> pure ReconnectWaitElapsed

waitForCommandReconnectSettle :: BridgeConfig -> UUID -> IO ReconnectWait
waitForCommandReconnectSettle config uuid = do
  debug config ("queued command pending; waiting for BlueZ reconnect settle " <> UUID.toString uuid <> ": delay_us=" <> show commandReconnectSettleMicros)
  threadDelay commandReconnectSettleMicros
  debug config ("queued command pending; reconnecting Sesame device now " <> UUID.toString uuid)
  pure ReconnectWaitInterruptedByCommand

waitReconnectDelay :: BridgeConfig -> UUID -> Int -> IO ()
waitReconnectDelay config uuid reconnectDelayCapMicros = do
  reconnectDelay <- randomReconnectDelayMicros reconnectDelayCapMicros
  debug config ("waiting after failed reconnect for " <> UUID.toString uuid <> ": delay_us=" <> show reconnectDelay <> ", cap_us=" <> show reconnectDelayCapMicros)
  threadDelay reconnectDelay

waitReconnectDelayAfterFailure :: BridgeConfig -> UUID -> Int -> SomeException -> IO Int
waitReconnectDelayAfterFailure config uuid reconnectDelayCapMicros err
  | isBluezLocalAbort err = do
      let reconnectDelayCapMicros' = max bluezLocalAbortReconnectDelayCapMicros reconnectDelayCapMicros
      reconnectDelay <- randomRIO (bluezLocalAbortMinReconnectDelayMicros, reconnectDelayCapMicros')
      debug config ("waiting after BlueZ local connection abort for " <> UUID.toString uuid <> ": delay_us=" <> show reconnectDelay <> ", cap_us=" <> show reconnectDelayCapMicros')
      threadDelay reconnectDelay
      pure reconnectDelayCapMicros'
  | otherwise = waitReconnectDelay config uuid reconnectDelayCapMicros *> pure reconnectDelayCapMicros

isBluezLocalAbort :: SomeException -> Bool
isBluezLocalAbort err =
  "le-connection-abort-by-local" `isInfixOf` show err

randomReconnectDelayMicros :: Int -> IO Int
randomReconnectDelayMicros reconnectDelayCapMicros =
  randomRIO (0, max 0 reconnectDelayCapMicros)

nextReconnectDelayMicros :: Int -> Int
nextReconnectDelayMicros reconnectDelayCapMicros =
  min maxReconnectDelayMicros (max minReconnectDelayMicros reconnectDelayCapMicros * 2)

waitForPendingCommandOrDelay :: PendingCommands -> UUID -> Int -> IO Bool
waitForPendingCommandOrDelay pendingCommands uuid timeoutMicros = do
  timedOut <- registerDelay timeoutMicros
  atomically
    ( ( do
          pending <- Map.member (deviceKey uuid) <$> readTVar pendingCommands
          check pending
          pure True
      )
        `orElse` do
          timedOutNow <- readTVar timedOut
          check timedOutNow
          pure False
    )

orTimeout :: STM Bool -> TVar Bool -> IO Bool
orTimeout waitAction timedOut =
  atomically
    ( waitAction
        `orElse` do
          timedOutNow <- readTVar timedOut
          check timedOutNow
          pure False
    )

debug :: BridgeConfig -> String -> IO ()
debug config message =
  if config.debugLogging
    then do
      timestamp <- currentTimestamp
      hPutStrLn stderr (timestamp <> " [haskesame-mqtt] " <> message)
    else pure ()

currentTimestamp :: IO String
currentTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" <$> getZonedTime

minReconnectDelayMicros :: Int
minReconnectDelayMicros = 10000

maxReconnectDelayMicros :: Int
maxReconnectDelayMicros = 3000000

bluezLocalAbortReconnectDelayCapMicros :: Int
bluezLocalAbortReconnectDelayCapMicros = 1000000

bluezLocalAbortMinReconnectDelayMicros :: Int
bluezLocalAbortMinReconnectDelayMicros = 500000

commandReconnectSettleMicros :: Int
commandReconnectSettleMicros = 250000

passiveReconnectSettleMicros :: Int
passiveReconnectSettleMicros = 1500000

commandStatusWaitMicros :: Int
commandStatusWaitMicros = 2500000

commandResponseGraceMicros :: Int
commandResponseGraceMicros = 1000000

commandSettleMicros :: Int
commandSettleMicros = 500000

queuedCommandSettleMicros :: Int
queuedCommandSettleMicros = 250000

commandIdleRefreshAge :: NominalDiffTime
commandIdleRefreshAge = 10

deviceKey :: UUID -> Text
deviceKey = T.pack . UUID.toString
