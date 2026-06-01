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
import Control.Concurrent.Async (async, forConcurrently_, race_)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay)
import Control.Exception.Safe qualified as Exception
import Control.Monad (forever)
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
  statusVersions <- newTVarIO Map.empty
  pendingCommands <- newTVarIO Map.empty
  atomically (mapM_ (\device -> STMMap.insert Nothing (deviceKey device.deviceUuid) deviceMap) devices)
  debug config ("starting bridge for " <> show (length devices) <> " Sesame device(s)")
  race_
    (consumeCommands mqtt config deviceMap statusVersions pendingCommands)
    (forConcurrently_ devices (superviseDevice mqtt config deviceMap statusVersions pendingCommands))

type DeviceMap = STMMap.Map Text (Maybe ConnectedBridgeDevice)

type StatusVersions = TVar (Map Text Int)

type PendingCommands = TVar (Map Text LockCommand)

superviseDevice :: Mqtt.AutoClient -> BridgeConfig -> DeviceMap -> StatusVersions -> PendingCommands -> BridgeDevice -> IO ()
superviseDevice mqtt config deviceMap statusVersions pendingCommands device =
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
        _ <- async (runPendingCommand config deviceMap statusVersions pendingCommands device.deviceUuid connected)
        publishResult <-
          Exception.tryAny
            ( publishDeviceStatus mqtt config statusVersions device.deviceUuid connected.sesameClient
                `Exception.finally` do
                  atomically (STMMap.insert Nothing (deviceKey device.deviceUuid) deviceMap)
                  _ <- Exception.tryAny connected.disconnectSesameClient
                  pure ()
            )
        debug config ("Sesame disconnected " <> UUID.toString device.deviceUuid <> ": " <> either show (const "status loop ended") publishResult)
        threadDelay reconnectDelayMicros

publishDeviceStatus :: Mqtt.AutoClient -> BridgeConfig -> StatusVersions -> UUID -> Sesame.Sesame5Client -> IO ()
publishDeviceStatus mqtt config statusVersions uuid sesame =
  forever do
    SesamePublish publishItem payload <- Sesame.readPublish sesame
    debug config ("received Sesame publish from " <> UUID.toString uuid <> ": " <> show publishItem)
    case publishItem of
      MechStatus ->
        case decodeSesame5MechStatus payload of
          Left err -> debug config ("failed to decode mech status from " <> UUID.toString uuid <> ": " <> show err)
          Right status -> do
            debug config ("publishing status for " <> UUID.toString uuid <> ": " <> describeMechStatus status)
            recordStatusVersion statusVersions uuid
            publishStatus mqtt config uuid (statusFromMech status)
      _ -> pure ()

consumeCommands :: Mqtt.AutoClient -> BridgeConfig -> DeviceMap -> StatusVersions -> PendingCommands -> IO ()
consumeCommands mqtt config devices statusVersions pendingCommands =
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
          Just (Just connected) -> executeCommand config devices statusVersions uuid connected command

executeCommand :: BridgeConfig -> DeviceMap -> StatusVersions -> UUID -> ConnectedBridgeDevice -> LockCommand -> IO ()
executeCommand config devices statusVersions uuid connected command = do
  beforeStatusVersion <- readStatusVersion statusVersions uuid
  result <- Exception.tryAny case command of
    CommandLock -> debug config ("locking " <> UUID.toString uuid) *> Sesame.lock connected.sesameClient config.historyName
    CommandUnlock -> debug config ("unlocking " <> UUID.toString uuid) *> Sesame.unlock connected.sesameClient config.historyName
  case result of
    Right () -> waitAfterCommand config statusVersions uuid beforeStatusVersion
    Left err -> do
      debug config ("command failed for " <> UUID.toString uuid <> ": " <> show err)
      atomically (STMMap.insert Nothing (deviceKey uuid) devices)
      _ <- Exception.tryAny connected.disconnectSesameClient
      pure ()

runPendingCommand :: BridgeConfig -> DeviceMap -> StatusVersions -> PendingCommands -> UUID -> ConnectedBridgeDevice -> IO ()
runPendingCommand config devices statusVersions pendingCommands uuid connected = do
  pending <- atomically do
    let key = deviceKey uuid
    command <- Map.lookup key <$> readTVar pendingCommands
    modifyTVar' pendingCommands (Map.delete key)
    pure command
  case pending of
    Nothing -> pure ()
    Just command -> do
      debug config ("running queued command for " <> UUID.toString uuid <> ": command=" <> show command)
      executeCommand config devices statusVersions uuid connected command

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

recordStatusVersion :: StatusVersions -> UUID -> IO ()
recordStatusVersion statusVersions uuid =
  atomically (modifyTVar' statusVersions (Map.insertWith (+) (deviceKey uuid) 1))

readStatusVersion :: StatusVersions -> UUID -> IO Int
readStatusVersion statusVersions uuid =
  Map.findWithDefault 0 (deviceKey uuid) <$> readTVarIO statusVersions

waitAfterCommand :: BridgeConfig -> StatusVersions -> UUID -> Int -> IO ()
waitAfterCommand config statusVersions uuid beforeStatusVersion = do
  debug config ("waiting for post-command Sesame status from " <> UUID.toString uuid)
  statusChanged <- waitForStatusVersion statusVersions uuid beforeStatusVersion commandStatusWaitMicros
  if statusChanged
    then debug config ("post-command Sesame status observed for " <> UUID.toString uuid)
    else debug config ("timed out waiting for post-command Sesame status from " <> UUID.toString uuid)
  threadDelay commandSettleMicros

shouldKeepDisconnectedCommand :: Mqtt.QoS -> Bool
shouldKeepDisconnectedCommand Mqtt.QoS0 = False
shouldKeepDisconnectedCommand Mqtt.QoS1 = True
shouldKeepDisconnectedCommand Mqtt.QoS2 = True

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

commandStatusWaitMicros :: Int
commandStatusWaitMicros = 3000000

commandSettleMicros :: Int
commandSettleMicros = 1500000

deviceKey :: UUID -> Text
deviceKey = T.pack . UUID.toString
