module Network.Sesame.Mqtt.SimpleBLE.App (
  AppConfig (..),
  MqttConfig (..),
  BridgeTomlConfig (..),
  DeviceConfig (..),
  configCodec,
  loadConfig,
  runApp,
) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Network.Mqtt.Client.AutoReconnect qualified as Mqtt
import Network.Sesame.Client qualified as Sesame
import Network.Sesame.Mqtt (BridgeConfig (..), BridgeDevice (..), runBridge)
import Network.Sesame.Transport (SesameTransport (..))
import Network.Sesame.Transport.SimpleBLE (SimpleBLEConfig (..), connectSimpleBLE, defaultSimpleBLEConfig)
import Network.Sesame.Types (Advertisement (..), SecretKey (..))
import Toml hiding (map)

data AppConfig = AppConfig
  { mqtt :: !MqttConfig
  , bridge :: !BridgeTomlConfig
  , devices :: ![DeviceConfig]
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec) via TomlTable AppConfig

data MqttConfig = MqttConfig
  { host :: !Text
  , port :: !Int
  , user :: !(Maybe Text)
  , password :: !(Maybe Text)
  , client_id :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable MqttConfig

data BridgeTomlConfig = BridgeTomlConfig
  { base_topic :: !Text
  , history_name :: !Text
  , debug_logging :: !(Maybe Bool)
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable BridgeTomlConfig

data DeviceConfig = DeviceConfig
  { uuid :: !(Maybe Text)
  , mac_address :: !Text
  , secret_key :: !Text
  , service_uuid :: !(Maybe Text)
  , write_characteristic_uuid :: !(Maybe Text)
  , notify_characteristic_uuid :: !(Maybe Text)
  , scan_timeout_ms :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable DeviceConfig

configCodec :: TomlCodec AppConfig
configCodec = genericCodec

runApp :: AppConfig -> IO ()
runApp config =
  Mqtt.withClient (mqttOptions config.mqtt) Mqtt.defaultAutoReconnectConfig \mqtt _ ->
    bracket (connectDevices config.devices) cleanupDevices \devices ->
      runBridge mqtt (bridgeConfig config.bridge) (map (.bridgeDevice) devices)

loadConfig :: FilePath -> IO AppConfig
loadConfig configFile =
  either (fail . show) pure =<< decodeFileExact configCodec configFile

mqttOptions :: MqttConfig -> Mqtt.ConnectOptions
mqttOptions config =
  case Mqtt.defaultConnectOptions connection clientId of
    Mqtt.ConnectOptions factory cid clean keepAlive _ _ will properties authenticator topicAliases queueBound overflow ->
      Mqtt.ConnectOptions factory cid clean keepAlive config.user (TE.encodeUtf8 <$> config.password) will properties authenticator topicAliases queueBound overflow
  where
    connection = Mqtt.tcpConnection (Mqtt.clientSettings (T.unpack config.host) (fromIntegral config.port))
    clientId = maybe "haskesame-mqtt-simpleble" id config.client_id

bridgeConfig :: BridgeTomlConfig -> BridgeConfig
bridgeConfig config =
  BridgeConfig
    { baseTopic = config.base_topic
    , historyName = config.history_name
    , debugLogging = maybe False id config.debug_logging
    }

data RunningDevice = RunningDevice
  { bridgeDevice :: !BridgeDevice
  , transport :: !SesameTransport
  }

connectDevices :: [DeviceConfig] -> IO [RunningDevice]
connectDevices = traverse connectDevice

connectDevice :: DeviceConfig -> IO RunningDevice
connectDevice config = do
  secret <- either fail pure (decodeHexText "secret_key" config.secret_key)
  transport <-
    either (fail . show) pure
      =<< connectSimpleBLE
        (simpleBLEConfig config)
  uuid <- deviceUuid config transport
  sesame <- Sesame.newSesame5Client transport
  _ <- Sesame.login sesame (SecretKey secret)
  pure
    RunningDevice
      { bridgeDevice = BridgeDevice uuid sesame
      , transport = transport
      }

cleanupDevices :: [RunningDevice] -> IO ()
cleanupDevices = mapM_ (.transport.closeBle)

simpleBLEConfig :: DeviceConfig -> SimpleBLEConfig
simpleBLEConfig config =
  defaults
    { serviceUuid = maybe defaults.serviceUuid id config.service_uuid
    , writeCharacteristicUuid = config.write_characteristic_uuid
    , notifyCharacteristicUuid = config.notify_characteristic_uuid
    , scanTimeoutMs = maybe defaults.scanTimeoutMs id config.scan_timeout_ms
    }
  where
    defaults = defaultSimpleBLEConfig config.mac_address

deviceUuid :: DeviceConfig -> SesameTransport -> IO UUID.UUID
deviceUuid config transport =
  case config.uuid of
    Just uuidText -> maybe (fail ("invalid Sesame UUID: " <> T.unpack uuidText)) pure (UUID.fromString (T.unpack uuidText))
    Nothing ->
      transport.advertisement >>= \case
        Right advertisement -> pure advertisement.deviceUuid
        Left err -> fail ("failed to discover Sesame UUID from advertisement: " <> show err)

decodeHexText :: String -> Text -> Either String ByteString
decodeHexText fieldName source =
  let stripped = T.filter (/= ' ') source
   in if T.length stripped `mod` 2 /= 0 || T.any (not . isHexDigit) stripped
        then Left ("invalid hex in " <> fieldName)
        else Right (BS.pack (go (T.unpack stripped)))
  where
    go (hi : lo : rest) = fromIntegral (digitToInt hi * 16 + digitToInt lo) : go rest
    go [] = []
    go [_] = error "decodeHexText: odd length checked above"
