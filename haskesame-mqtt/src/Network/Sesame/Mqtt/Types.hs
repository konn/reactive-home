module Network.Sesame.Mqtt.Types (
  BridgeConfig (..),
  ConnectedBridgeDevice (..),
  BridgeDevice (..),
  BridgeError (..),
  LockCommand (..),
  LockState (..),
  StatusPayload (..),
  defaultBridgeConfig,
  statusFromMech,
  encodeStatusPayload,
) where

import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Bits (Bits, (.&.))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.Sesame.Client (Sesame5Client)
import Network.Sesame.Types (Sesame5MechStatus, batteryPercentage)
import Network.Sesame.Types qualified as Sesame

data BridgeConfig = BridgeConfig
  { baseTopic :: !Text
  , historyName :: !Text
  , debugLogging :: !Bool
  }
  deriving stock (Show, Eq, Generic)

defaultBridgeConfig :: BridgeConfig
defaultBridgeConfig =
  BridgeConfig
    { baseTopic = "ssm2mqtt"
    , historyName = "ssm2mqtt"
    , debugLogging = False
    }

data BridgeDevice = BridgeDevice
  { deviceUuid :: !UUID
  , connectSesameClient :: !(IO ConnectedBridgeDevice)
  }
  deriving stock (Generic)

data ConnectedBridgeDevice = ConnectedBridgeDevice
  { sesameClient :: !Sesame5Client
  , disconnectSesameClient :: !(IO ())
  , abortSesameClient :: !(IO ())
  }
  deriving stock (Generic)

data LockCommand = CommandLock | CommandUnlock
  deriving stock (Show, Eq, Ord, Generic)

data LockState = Locked | Unlocked
  deriving stock (Show, Eq, Ord, Generic)

data StatusPayload = StatusPayload
  { position :: !Int
  , lockCurrentState :: !LockState
  , batteryVoltage :: !Double
  , batteryLevel :: !Int
  , chargingState :: !Text
  , statusLowBattery :: !Bool
  }
  deriving stock (Show, Eq, Generic)

data BridgeError
  = InvalidBaseTopic !Text
  | InvalidTopic !Text
  | InvalidCommandPayload !Text
  | UnknownDevice !UUID
  deriving stock (Show, Eq, Generic)

instance ToJSON LockState where
  toJSON Locked = String "LOCKED"
  toJSON Unlocked = String "UNLOCKED"

instance ToJSON StatusPayload where
  toJSON status =
    object
      [ "position" .= status.position
      , "lockCurrentState" .= status.lockCurrentState
      , "batteryVoltage" .= status.batteryVoltage
      , "batteryLevel" .= status.batteryLevel
      , "chargingState" .= status.chargingState
      , "statusLowBattery" .= status.statusLowBattery
      ]

statusFromMech :: Sesame5MechStatus -> StatusPayload
statusFromMech mech =
  StatusPayload
    { position = fromIntegral mech.position
    , lockCurrentState = if isInLockRange mech then Locked else Unlocked
    , batteryVoltage = Sesame.batteryVoltage mech
    , batteryLevel = batteryPercentage mech
    , chargingState = "NOT_CHARGEABLE"
    , statusLowBattery = isBatteryCritical mech
    }

encodeStatusPayload :: StatusPayload -> LBS.ByteString
encodeStatusPayload = Aeson.encode

isInLockRange :: Sesame5MechStatus -> Bool
isInLockRange mech = mech.statusFlags `hasFlag` 0x02

isBatteryCritical :: Sesame5MechStatus -> Bool
isBatteryCritical mech = mech.statusFlags `hasFlag` 0x20

hasFlag :: (Bits a, Num a) => a -> a -> Bool
hasFlag value flag = value .&. flag /= 0
