{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Home.Reactive.Sesame5 (
  SesameDevice (..),
  SesameConfig (..),
  SesameUUID (..),
  SesameEnv (..),
  LockStatus (..),
  RawSesameStatus (..),
  SesameStatus (..),
  SesameError (..),
  fromSesameConfig,
  sesameTopicFilters,
  parseSesameStatus,
  sesameStatuses,
  buildSesameStatus,
  aggregateSesameStatus,
) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text qualified as T
import Effectful
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.App.Types (ParseResult (..))
import Home.Reactive.MQTT
import Home.Reactive.Metrics.Mackerel
import Network.Mqtt.Types.Topic (stripPrefix)
import Toml hiding (map)

data SesameDevice = SesameDevice {uuid :: SesameUUID}
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable SesameDevice

data SesameConfig = SesameConfig
  { prefix :: !T.Text
  , devices :: !(HashMap T.Text SesameDevice)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable SesameConfig

fromSesameConfig :: SesameConfig -> SesameEnv
fromSesameConfig SesameConfig {..} =
  SesameEnv
    { prefix = prefix
    , devices = devices
    , uuids = HM.fromList $ map (\(name, dev) -> (dev.uuid, (name, dev))) $ HM.toList devices
    }

newtype SesameUUID = UUID {raw :: T.Text}
  deriving stock (Generic)
  deriving newtype
    ( IsString
    , Show
    , Eq
    , Ord
    , HasItemCodec
    , HasCodec
    , Hashable
    )

data SesameEnv = SesameEnv
  { prefix :: !T.Text
  , devices :: !(HashMap T.Text SesameDevice)
  , uuids :: !(HashMap SesameUUID (T.Text, SesameDevice))
  }
  deriving (Show, Eq, Ord, Generic)

sesameTopicFilters :: SesameConfig -> [TopicFilter]
sesameTopicFilters SesameConfig {..}
  | null devices =
      [ TopicFilter prefix <> wildOne <> action
      | action <- ["set", "get"]
      ]
  | otherwise =
      [ TopicFilter prefix <> TopicFilter device.uuid.raw <> action
      | device <- F.toList devices
      , action <- ["set", "get"]
      ]

data LockStatus = LOCKED | UNLOCKED
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RawSesameStatus = SesamePayload
  { position :: {-# UNPACK #-} !Int
  , lockCurrentState :: {-# UNPACK #-} !LockStatus
  , batteryVoltage :: {-# UNPACK #-} !Double
  , batteryLevel :: {-# UNPACK #-} !Int
  , chargingState :: {-# UNPACK #-} !T.Text
  , statusLowBattery :: {-# UNPACK #-} !Bool
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SesameStatus = SesameStatus
  { name :: {-# UNPACK #-} !T.Text
  , uuid :: {-# UNPACK #-} !SesameUUID
  , lastUpdated :: {-# UNPACK #-} !UTCTime
  , position :: {-# UNPACK #-} !Int
  , lockCurrentState :: {-# UNPACK #-} !LockStatus
  , batteryVoltage :: {-# UNPACK #-} !Double
  , batteryLevel :: {-# UNPACK #-} !Int
  , statusLowBattery :: {-# UNPACK #-} !Bool
  }
  deriving (Show, Eq, Ord, Generic)

data SesameError
  = InvalidMessage !MqttMessage
  | UnknownDevice !T.Text
  | InvalidPayload !String
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

parseSesameStatus ::
  SesameEnv ->
  MqttMessage ->
  ParseResult SesameError (T.Text, SesameDevice, RawSesameStatus)
parseSesameStatus env msg =
  case stripPrefix env.prefix msg.topic of
    Nothing -> Skipped
    Just (Topic rest)
      | (uuid, slashAction) <- T.breakOn "/" rest
      , let action = T.drop 1 slashAction
      , not (T.null action) ->
          case action of
            "get"
              | Just (name, device) <- HM.lookup (UUID uuid) env.uuids ->
                  case A.eitherDecode' $ LBS.fromStrict msg.payload of
                    Left err -> ParseFailure $ InvalidPayload err
                    Right stt -> ParseSuccess (name, device, stt)
              | otherwise -> ParseFailure (UnknownDevice uuid)
            _ -> Skipped
      | otherwise -> Skipped

sesameStatuses ::
  BehaviourF
    (Eff es)
    UTCTime
    (Maybe SesameEnv, MqttMessage)
    (ParseResult SesameError SesameStatus)
sesameStatuses = proc (msess, msg) -> do
  TimeInfo {..} <- timeInfo -< ()
  returnA
    -< case msess of
      Nothing -> Skipped
      Just sess ->
        parseSesameStatus sess msg
          <&> \(name, device, stat) ->
            buildSesameStatus absolute name device stat

buildSesameStatus ::
  UTCTime ->
  T.Text ->
  SesameDevice ->
  RawSesameStatus ->
  SesameStatus
buildSesameStatus absolute name device stat =
  SesameStatus
    { name
    , uuid = device.uuid
    , lastUpdated = absolute
    , position = stat.position
    , lockCurrentState = stat.lockCurrentState
    , batteryVoltage = stat.batteryVoltage
    , batteryLevel = stat.batteryLevel
    , statusLowBattery = stat.statusLowBattery
    }

aggregateSesameStatus ::
  BehaviourF (Eff es) UTCTime (Maybe SesameStatus) (HashMap T.Text SesameStatus)
aggregateSesameStatus =
  feedback HM.empty $ arr \(mmsg, prev) ->
    let new = case mmsg of
          Nothing -> prev
          Just stt -> HM.insert stt.name stt prev
     in (new, new)

instance ToMackerelMetrics SesameStatus where
  toMetrics stt =
    [ MackerelEntry
        { name = "sesame.position." <> stt.name
        , time = stt.lastUpdated
        , value = A.toJSON stt.position
        }
    , MackerelEntry
        { name = "sesame.lockCurrentState." <> stt.name
        , time = stt.lastUpdated
        , value = case stt.lockCurrentState of
            LOCKED -> A.Number 1
            UNLOCKED -> A.Number 0
        }
    , MackerelEntry
        { name = "sesame.batteryVoltage." <> stt.name
        , time = stt.lastUpdated
        , value = A.toJSON stt.batteryVoltage
        }
    , MackerelEntry
        { name = "sesame.batteryLevel." <> stt.name
        , time = stt.lastUpdated
        , value = A.toJSON stt.batteryLevel
        }
    , MackerelEntry
        { name = "sesame.statusLowBattery." <> stt.name
        , time = stt.lastUpdated
        , value = if stt.statusLowBattery then A.Number 1 else A.Number 0
        }
    ]
