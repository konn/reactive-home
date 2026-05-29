module Network.Sesame.Mqtt.Bluez.App (
  AppConfig (..),
  MqttConfig (..),
  BridgeTomlConfig (..),
  DeviceConfig (..),
  configCodec,
  loadConfig,
  runApp,
) where

import Control.Exception (bracket)
import DBus (objectPath_)
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
import Network.Sesame.Transport.Bluez (BluezConfig (..), connectBluez)
import Network.Sesame.Types (SecretKey (..))
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
  }
  deriving stock (Show, Eq, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable BridgeTomlConfig

data DeviceConfig = DeviceConfig
  { uuid :: !Text
  , secret_key :: !Text
  , device_path :: !Text
  , write_characteristic_path :: !Text
  , notify_characteristic_path :: !Text
  , manufacturer_data :: !(Maybe Text)
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
    clientId = maybe "haskesame-mqtt-bluez" id config.client_id

bridgeConfig :: BridgeTomlConfig -> BridgeConfig
bridgeConfig config =
  BridgeConfig
    { baseTopic = config.base_topic
    , historyName = config.history_name
    }

data RunningDevice = RunningDevice
  { bridgeDevice :: !BridgeDevice
  , transport :: !SesameTransport
  }

connectDevices :: [DeviceConfig] -> IO [RunningDevice]
connectDevices = traverse connectDevice

connectDevice :: DeviceConfig -> IO RunningDevice
connectDevice config = do
  uuid <- maybe (fail ("invalid Sesame UUID: " <> T.unpack config.uuid)) pure (UUID.fromString (T.unpack config.uuid))
  secret <- either fail pure (decodeHexText "secret_key" config.secret_key)
  manufacturer <- either fail pure (traverse (decodeHexText "manufacturer_data") config.manufacturer_data)
  transport <-
    either (fail . show) pure
      =<< connectBluez
        BluezConfig
          { devicePath = objectPath_ (T.unpack config.device_path)
          , writeCharacteristicPath = objectPath_ (T.unpack config.write_characteristic_path)
          , notifyCharacteristicPath = objectPath_ (T.unpack config.notify_characteristic_path)
          , manufacturerData = manufacturer
          }
  sesame <- Sesame.newSesame5Client transport
  _ <- Sesame.login sesame (SecretKey secret)
  pure
    RunningDevice
      { bridgeDevice = BridgeDevice uuid sesame
      , transport = transport
      }

cleanupDevices :: [RunningDevice] -> IO ()
cleanupDevices = mapM_ (.transport.closeBle)

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
