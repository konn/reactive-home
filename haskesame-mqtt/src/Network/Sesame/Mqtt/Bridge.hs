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
import Control.Concurrent.Async (async, forConcurrently_, race_, withAsync)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay)
import Control.Exception.Safe qualified as Exception
import Control.Monad (forever, when)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.Mqtt.Client.AutoReconnect qualified as Mqtt
import Network.Sesame.Client qualified as Sesame
import Network.Sesame.Codec (decodeSesame5MechStatus)
import Network.Sesame.Mqtt.Types
import Network.Sesame.Types (ItemCode (MechStatus), Sesame5MechStatus (..), SesamePublish (..))
import StmContainers.Map qualified as STMMap
import System.IO (hPutStrLn, stderr)

runBridge :: Mqtt.AutoClient -> BridgeConfig -> [BridgeDevice] -> IO ()
runBridge mqtt config devices = do
  filter_ <- either (fail . show) pure (commandFilter config)
  debug config ("subscribing command topic filter: " <> show filter_)
  _ <- Mqtt.subscribe1 mqtt filter_ Mqtt.QoS1
  deviceMap <- STMMap.newIO
  commandLocks <- Map.fromList <$> traverse (\device -> (deviceKey device.deviceUuid,) <$> newMVar ()) devices
  statusVersions <- newTVarIO Map.empty
  statusSnapshots <- newTVarIO Map.empty
  pendingCommands <- newTVarIO Map.empty
  atomically (mapM_ (\device -> STMMap.insert Nothing (deviceKey device.deviceUuid) deviceMap) devices)
  debug config ("starting bridge for " <> show (length devices) <> " Sesame device(s)")
  race_
    (consumeCommands mqtt config deviceMap commandLocks statusVersions statusSnapshots pendingCommands)
    (forConcurrently_ devices (superviseDevice mqtt config deviceMap commandLocks statusVersions statusSnapshots pendingCommands))

type DeviceMap = STMMap.Map Text (Maybe ConnectedBridgeDevice)

type CommandLocks = Map Text (MVar ())

type StatusVersions = TVar (Map Text Int)

type StatusSnapshots = TVar (Map Text StatusPayload)

type PendingCommands = TVar (Map Text LockCommand)

superviseDevice :: Mqtt.AutoClient -> BridgeConfig -> DeviceMap -> CommandLocks -> StatusVersions -> StatusSnapshots -> PendingCommands -> BridgeDevice -> IO ()
superviseDevice mqtt config deviceMap commandLocks statusVersions statusSnapshots pendingCommands device =
  forever do
    debug config ("connecting Sesame device " <> UUID.toString device.deviceUuid)
    connectedResult <- Exception.tryAny device.connectSesameClient
    case connectedResult of
      Left err -> do
        debug config ("Sesame connection failed for " <> UUID.toString device.deviceUuid <> ": " <> show err)
        threadDelay reconnectDelayMicros
      Right connected -> do
        debug config ("Sesame connected " <> UUID.toString device.deviceUuid)
        atomically (STMMap.insert (Just connected) (deviceKey device.deviceUuid) deviceMap)
        publishResult <-
          Exception.tryAny
            ( withAsync
                (runPendingCommand config deviceMap commandLocks statusVersions statusSnapshots pendingCommands device.deviceUuid connected)
                \_ ->
                  publishDeviceStatus mqtt config statusVersions statusSnapshots device.deviceUuid connected.sesameClient
                    `Exception.finally` do
                      atomically (STMMap.insert Nothing (deviceKey device.deviceUuid) deviceMap)
                      _ <- Exception.tryAny connected.disconnectSesameClient
                      pure ()
            )
        debug config ("Sesame disconnected " <> UUID.toString device.deviceUuid <> ": " <> either show (const "status loop ended") publishResult)
        waitBeforeReconnect config pendingCommands device.deviceUuid

publishDeviceStatus :: Mqtt.AutoClient -> BridgeConfig -> StatusVersions -> StatusSnapshots -> UUID -> Sesame.Sesame5Client -> IO ()
publishDeviceStatus mqtt config statusVersions statusSnapshots uuid sesame =
  forever do
    SesamePublish publishItem payload <- Sesame.readPublish sesame
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

consumeCommands :: Mqtt.AutoClient -> BridgeConfig -> DeviceMap -> CommandLocks -> StatusVersions -> StatusSnapshots -> PendingCommands -> IO ()
consumeCommands mqtt config devices commandLocks statusVersions statusSnapshots pendingCommands =
  forever do
    message <- Mqtt.recvMessage mqtt
    case parseCommandMessage config message of
      Left err -> debug config ("ignoring MQTT command: " <> show err)
      Right (uuid, command) -> do
        debug config ("received MQTT command for " <> UUID.toString uuid <> ": command=" <> show command <> ", qos=" <> show message.qos <> ", retain=" <> show message.retain <> ", dup=" <> show message.dup)
        atomically (STMMap.lookup (deviceKey uuid) devices) >>= \case
          Nothing -> debug config ("ignoring command for unknown Sesame device: " <> UUID.toString uuid)
          Just Nothing ->
            if shouldKeepDisconnectedCommand message.qos
              then do
                atomically (modifyTVar' pendingCommands (Map.insert (deviceKey uuid) command))
                debug config ("queued disconnected command for " <> UUID.toString uuid <> ": command=" <> show command <> ", qos=" <> show message.qos)
              else debug config ("ignoring QoS0 command while Sesame device is disconnected: " <> UUID.toString uuid)
          Just (Just connected) -> startCommand config devices commandLocks statusVersions statusSnapshots pendingCommands uuid connected command

startCommand :: BridgeConfig -> DeviceMap -> CommandLocks -> StatusVersions -> StatusSnapshots -> PendingCommands -> UUID -> ConnectedBridgeDevice -> LockCommand -> IO ()
startCommand config devices commandLocks statusVersions statusSnapshots pendingCommands uuid connected command =
  case Map.lookup (deviceKey uuid) commandLocks of
    Nothing -> do
      _ <- async (commandWorker config devices statusVersions statusSnapshots pendingCommands Nothing uuid connected command)
      pure ()
    Just commandLock ->
      tryTakeMVar commandLock >>= \case
        Nothing -> queuePendingCommand config pendingCommands uuid command "command already in progress"
        Just () -> do
          _ <- async (commandWorker config devices statusVersions statusSnapshots pendingCommands (Just commandLock) uuid connected command)
          pure ()

commandWorker :: BridgeConfig -> DeviceMap -> StatusVersions -> StatusSnapshots -> PendingCommands -> Maybe (MVar ()) -> UUID -> ConnectedBridgeDevice -> LockCommand -> IO ()
commandWorker config devices statusVersions statusSnapshots pendingCommands commandLock uuid connected command = do
  result <- Exception.tryAny (drain command)
  case commandLock of
    Nothing -> pure ()
    Just lock -> do
      putMVar lock ()
      when (either (const False) id result) do
        restartCommandWorkerIfPending config devices statusVersions statusSnapshots pendingCommands uuid connected lock
  either Exception.throwIO (const (pure ())) result
  where
    drain nextCommand = do
      transportUsable <- executeCommandUnlocked config devices statusVersions statusSnapshots pendingCommands uuid connected nextCommand
      if transportUsable
        then
          takePendingCommand pendingCommands uuid >>= \case
            Nothing -> pure True
            Just pendingCommand -> do
              debug config ("running pending command for " <> UUID.toString uuid <> ": command=" <> show pendingCommand)
              drain pendingCommand
        else pure False

executeCommandUnlocked :: BridgeConfig -> DeviceMap -> StatusVersions -> StatusSnapshots -> PendingCommands -> UUID -> ConnectedBridgeDevice -> LockCommand -> IO Bool
executeCommandUnlocked config devices statusVersions statusSnapshots pendingCommands uuid connected command = do
  readStatusSnapshot statusSnapshots uuid >>= \case
    Just status
      | commandSatisfied command status ->
          debug config ("skipping command already satisfied for " <> UUID.toString uuid <> ": command=" <> show command <> ", status=" <> show status) *> pure True
    _ -> do
      beforeStatusVersion <- readStatusVersion statusVersions uuid
      result <- Exception.tryAny case command of
        CommandLock -> debug config ("locking " <> UUID.toString uuid) *> Sesame.lock connected.sesameClient config.historyName
        CommandUnlock -> debug config ("unlocking " <> UUID.toString uuid) *> Sesame.unlock connected.sesameClient config.historyName
      case result of
        Right () -> do
          statusSatisfied <- waitAfterCommand config statusVersions statusSnapshots uuid command beforeStatusVersion
          if statusSatisfied
            then pure True
            else requeueFailedCommand config devices pendingCommands uuid connected command "post-command status timeout" *> pure False
        Left err -> do
          debug config ("command failed for " <> UUID.toString uuid <> ": " <> show err)
          requeueFailedCommand config devices pendingCommands uuid connected command "command failure"
          pure False

runPendingCommand :: BridgeConfig -> DeviceMap -> CommandLocks -> StatusVersions -> StatusSnapshots -> PendingCommands -> UUID -> ConnectedBridgeDevice -> IO ()
runPendingCommand config devices commandLocks statusVersions statusSnapshots pendingCommands uuid connected = do
  pendingAtConnect <- hasPendingCommand pendingCommands uuid
  if pendingAtConnect
    then do
      beforeInitialStatus <- readStatusVersion statusVersions uuid
      debug config ("waiting for initial Sesame status before queued command for " <> UUID.toString uuid)
      initialStatusSeen <- waitForStatusVersion statusVersions uuid beforeInitialStatus reconnectStatusWaitMicros
      if initialStatusSeen
        then debug config ("initial Sesame status observed before queued command for " <> UUID.toString uuid)
        else debug config ("timed out waiting for initial Sesame status before queued command for " <> UUID.toString uuid)
      threadDelay commandSettleMicros
    else pure ()
  pending <- atomically do
    let key = deviceKey uuid
    command <- Map.lookup key <$> readTVar pendingCommands
    modifyTVar' pendingCommands (Map.delete key)
    pure command
  case pending of
    Nothing -> pure ()
    Just command -> do
      debug config ("running queued command for " <> UUID.toString uuid <> ": command=" <> show command)
      startCommand config devices commandLocks statusVersions statusSnapshots pendingCommands uuid connected command

hasPendingCommand :: PendingCommands -> UUID -> IO Bool
hasPendingCommand pendingCommands uuid =
  Map.member (deviceKey uuid) <$> readTVarIO pendingCommands

queuePendingCommand :: BridgeConfig -> PendingCommands -> UUID -> LockCommand -> String -> IO ()
queuePendingCommand config pendingCommands uuid command reason = do
  atomically (modifyTVar' pendingCommands (Map.insert (deviceKey uuid) command))
  debug config ("queued latest command because " <> reason <> " for " <> UUID.toString uuid <> ": command=" <> show command)

takePendingCommand :: PendingCommands -> UUID -> IO (Maybe LockCommand)
takePendingCommand pendingCommands uuid =
  atomically do
    let key = deviceKey uuid
    command <- Map.lookup key <$> readTVar pendingCommands
    modifyTVar' pendingCommands (Map.delete key)
    pure command

restartCommandWorkerIfPending :: BridgeConfig -> DeviceMap -> StatusVersions -> StatusSnapshots -> PendingCommands -> UUID -> ConnectedBridgeDevice -> MVar () -> IO ()
restartCommandWorkerIfPending config devices statusVersions statusSnapshots pendingCommands uuid connected commandLock = do
  pending <- hasPendingCommand pendingCommands uuid
  when pending do
    tryTakeMVar commandLock >>= \case
      Nothing -> pure ()
      Just () ->
        takePendingCommand pendingCommands uuid >>= \case
          Nothing -> putMVar commandLock ()
          Just command -> do
            debug config ("restarting command worker for pending command " <> UUID.toString uuid <> ": command=" <> show command)
            _ <- async (commandWorker config devices statusVersions statusSnapshots pendingCommands (Just commandLock) uuid connected command)
            pure ()

requeueFailedCommand :: BridgeConfig -> DeviceMap -> PendingCommands -> UUID -> ConnectedBridgeDevice -> LockCommand -> String -> IO ()
requeueFailedCommand config devices pendingCommands uuid connected command reason = do
  requeued <-
    atomically do
      let key = deviceKey uuid
      pending <- Map.member key <$> readTVar pendingCommands
      requeued <-
        if pending
          then pure False
          else modifyTVar' pendingCommands (Map.insert key command) *> pure True
      STMMap.insert Nothing key devices
      pure requeued
  if requeued
    then debug config ("requeued command after " <> reason <> " for " <> UUID.toString uuid <> ": command=" <> show command)
    else debug config ("left newer pending command after " <> reason <> " for " <> UUID.toString uuid <> ": failed_command=" <> show command)
  _ <- Exception.tryAny connected.disconnectSesameClient
  pure ()

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

readStatusVersion :: StatusVersions -> UUID -> IO Int
readStatusVersion statusVersions uuid =
  Map.findWithDefault 0 (deviceKey uuid) <$> readTVarIO statusVersions

readStatusSnapshot :: StatusSnapshots -> UUID -> IO (Maybe StatusPayload)
readStatusSnapshot statusSnapshots uuid =
  Map.lookup (deviceKey uuid) <$> readTVarIO statusSnapshots

waitAfterCommand :: BridgeConfig -> StatusVersions -> StatusSnapshots -> UUID -> LockCommand -> Int -> IO Bool
waitAfterCommand config statusVersions statusSnapshots uuid command beforeStatusVersion = do
  debug config ("waiting for post-command Sesame status from " <> UUID.toString uuid <> ": command=" <> show command)
  statusSatisfied <- waitForCommandStatus statusVersions statusSnapshots uuid command beforeStatusVersion commandStatusWaitMicros
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

waitForStatusVersion :: StatusVersions -> UUID -> Int -> Int -> IO Bool
waitForStatusVersion statusVersions uuid beforeStatusVersion timeoutMicros = do
  timedOut <- registerDelay timeoutMicros
  ( do
      versions <- readTVar statusVersions
      check (Map.findWithDefault 0 (deviceKey uuid) versions > beforeStatusVersion)
      pure True
    )
    `orTimeout` timedOut

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

waitBeforeReconnect :: BridgeConfig -> PendingCommands -> UUID -> IO ()
waitBeforeReconnect config pendingCommands uuid = do
  reconnectNow <- waitForPendingCommandOrDelay pendingCommands uuid reconnectDelayMicros
  if reconnectNow
    then debug config ("queued command pending; reconnecting Sesame device now " <> UUID.toString uuid)
    else pure ()

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

reconnectDelayMicros :: Int
reconnectDelayMicros = 5000000

reconnectStatusWaitMicros :: Int
reconnectStatusWaitMicros = 5000000

commandStatusWaitMicros :: Int
commandStatusWaitMicros = 3000000

commandSettleMicros :: Int
commandSettleMicros = 1500000

deviceKey :: UUID -> Text
deviceKey = T.pack . UUID.toString
