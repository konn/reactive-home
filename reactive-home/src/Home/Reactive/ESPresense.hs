{-# LANGUAGE ApplicativeDo #-}
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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.ESPresense (
  ESPSensor (..),
  Duration (),
  millis,
  seconds,
  minutes,
  hours,
  days,
  ESPresenseConfig (..),
  Room (..),
  SensorCondition (..),
  ESPSensorName,
  ESPDeviceId,
  espresenseConfigCodec,
  RawESPStatus (..),
  ESPStatus (..),
  ESPSensorState (..),
  initialiseRooms,
  espresenseTopicFilters,
  parseRawESPStatus,
  parseESPStatusS,
  buildESPStatus,
  aggregateESPStatus,
  occupancyListS,
  OccupancyList,
  isVacant,
  occupants,
  numOccupants,
) where

import Control.Applicative (empty, (<|>))
import Control.Lens ((&), (.~))
import Control.Monad (unless, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Foldable (find)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List ((\\))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Network.Mqtt (Mqtt, QoS (..), defaultPublishOptions, publish)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.App.Types (ParseResult (..))
import Home.Reactive.MQTT
import Home.Reactive.Metrics.Mackerel
import Network.Mqtt.Types.Topic (stripPrefix)
import Text.Read (readEither)
import Toml hiding (first, map)
import Validation (Validation (..))

data ESPSensor = ESPSensor
  { name :: !T.Text
  , max_distance :: !Float
  , skip_distance :: !Float
  , skip_ms :: !Int
  , timeout :: !Duration
  , window :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic)

option :: b -> Key -> TomlCodec b -> TomlCodec b
option def key codec =
  Codec
    { codecRead = \toml ->
        if HM.member key toml.tomlPairs
          then codecRead codec toml
          else pure def
    , codecWrite = codecWrite codec
    }

numberFloat :: Key -> TomlCodec Float
numberFloat key = Toml.float key <|> Toml.dimap round fromIntegral (Toml.int key)

roomCodec :: Codec ESPSensor ESPSensor
roomCodec = do
  name <- Toml.text "name" .= (.name)
  max_distance <- option 16 "max_distance" (numberFloat "max_distance") .= (.max_distance)
  skip_distance <- option 0.5 "skip_distance" (numberFloat "skip_distance") .= (.skip_distance)
  skip_ms <- option 5000 "skip_ms" (Toml.int "skip_ms") .= (.skip_ms)
  timeout <- option (Duration 5) "timeout" (Toml.textBy formatDuration parseDuration "timeout") .= (.timeout)
  window <- Toml.dioptional (Toml.int "window") .= (.window)
  pure ESPSensor {..}

instance HasItemCodec ESPSensor where
  hasItemCodec = Right roomCodec

instance HasCodec ESPSensor where
  hasCodec = table roomCodec

data ESPresenseConfig = ESPresenseConfig
  { devices :: ![T.Text]
  , sensors :: ![ESPSensor]
  , rooms :: !(HashMap T.Text Room)
  }
  deriving (Show, Eq, Ord, Generic)

instance HasCodec ESPresenseConfig where
  hasCodec = Toml.table espresenseConfigCodec

instance HasItemCodec ESPresenseConfig where
  hasItemCodec = Right espresenseConfigCodec

data SensorCondition = SensorCondition
  { name :: !T.Text
  , distance :: !Float
  }
  deriving (Show, Eq, Ord, Generic)

sensorConditionCodec :: TomlCodec SensorCondition
sensorConditionCodec = do
  name <- Toml.text "name" .= (.name)
  distance <- numberFloat "distance" .= (.distance)
  pure SensorCondition {..}

data Room = Room
  { devices :: ![T.Text]
  , leave :: ![SensorCondition]
  , entry :: ![SensorCondition]
  }
  deriving (Show, Eq, Ord, Generic)

conditionsCodec :: TomlCodec [SensorCondition]
conditionsCodec = Toml.list sensorConditionCodec "conditions"

roomRuleCodec :: TomlCodec Room
roomRuleCodec = do
  devices <- Toml.arrayOf Toml._Text "devices" .= (.devices)
  leave <- Toml.table conditionsCodec "leave" .= (.leave)
  entry <- entryCodec .= (.entry)
  pure Room {..}
  where
    entryCodec :: TomlCodec [SensorCondition]
    entryCodec =
      Codec
        { codecRead =
            codecRead returnCodec
              <!> codecRead legacyEntryCodec
        , codecWrite = \conditions ->
            conditions
              <$ ( codecWrite returnCodec conditions
                     *> codecWrite legacyEntryCodec conditions
                 )
        }
    returnCodec = Toml.table conditionsCodec "return"
    legacyEntryCodec = Toml.table conditionsCodec "entry"

espresenseConfigCodec :: TomlCodec ESPresenseConfig
espresenseConfigCodec = validateConfigCodec baseCodec
  where
    baseCodec :: TomlCodec ESPresenseConfig
    baseCodec = do
      devices <- Toml.arrayOf Toml._Text "devices" .= (.devices)
      sensors <- Toml.list roomCodec "sensors" .= (.sensors)
      rooms <- roomsCodec .= (.rooms)
      pure ESPresenseConfig {..}

    roomsCodec :: TomlCodec (HashMap T.Text Room)
    roomsCodec =
      Codec
        { codecRead = \toml ->
            HM.fromList
              <$> traverse
                readRoom
                [ (roomName, roomToml)
                | (Toml.Piece "rooms" :|| [Toml.Piece "rooms", Toml.Piece roomName], roomToml) <- Toml.toList (toml.tomlTables)
                ]
        , codecWrite = \rooms ->
            TomlState $ \toml ->
              ( Just rooms
              , foldl' insertRoom toml (HM.toList rooms)
              )
        }

    readRoom :: (T.Text, TOML) -> Validation [TomlDecodeError] (T.Text, Room)
    readRoom (roomName, roomToml) =
      (\room -> (roomName, room)) <$> codecRead roomRuleCodec roomToml

    insertRoom :: TOML -> (T.Text, Room) -> TOML
    insertRoom toml (roomName, room) =
      Toml.insertTable
        (Toml.Piece "rooms" :|| [Toml.Piece roomName])
        (Toml.execTomlCodec roomRuleCodec room)
        toml

validateConfigCodec :: TomlCodec ESPresenseConfig -> TomlCodec ESPresenseConfig
validateConfigCodec codec =
  Codec
    { codecRead = \toml -> do
        case codecRead codec toml of
          Failure errs -> Failure errs
          Success cfg ->
            case firstInvalidRoomDevices cfg of
              Nothing -> pure cfg
              Just _ -> empty
    , codecWrite = codecWrite codec
    }

firstInvalidRoomDevices :: ESPresenseConfig -> Maybe (T.Text, [T.Text])
firstInvalidRoomDevices cfg =
  find (not . null . snd) $
    [ (roomName, room.devices \\ cfg.devices)
    | (roomName, room) <- HM.toList cfg.rooms
    ]

newtype Duration = Duration {seconds :: Diff UTCTime}
  deriving (Eq, Ord, Generic)

millis :: Double -> Duration
millis ms = Duration (ms / 1000)

seconds :: Double -> Duration
seconds = Duration

minutes :: Double -> Duration
minutes m = Duration (m * 60)

hours :: Double -> Duration
hours h = Duration (h * 3600)

days :: Double -> Duration
days d = Duration (d * 86400)

instance Show Duration where
  show (Duration secs) = show secs ++ "s"

instance HasCodec Duration where
  hasCodec = textBy formatDuration parseDuration

initialiseRooms ::
  (Mqtt :> es) =>
  [ESPDeviceId] ->
  [ESPSensor] ->
  Eff es ()
initialiseRooms sensors = mapM_ (initialiseRoom sensors)

pub :: (Mqtt :> es) => Topic -> BS8.ByteString -> Eff es ()
pub topic payload = do
  let opts = defaultPublishOptions & #qos .~ QoS1
  void $ publish topic payload opts

initialiseRoom ::
  (Mqtt :> es) =>
  [ESPDeviceId] ->
  ESPSensor ->
  Eff es ()
initialiseRoom sensors room = do
  let setTopic k = "espresense" <> "rooms" <> Topic room.name <> Topic k <> "set"
  pub (setTopic "max_distance") (BS8.pack $ show room.max_distance)
  pub (setTopic "skip_distance") (BS8.pack $ show room.skip_distance)
  pub (setTopic "skip_ms") (BS8.pack $ show room.skip_ms)

  unless (null sensors) $ do
    void $ pub (setTopic "include") (BS8.intercalate " " (map TE.encodeUtf8 sensors))
    void $ pub (setTopic "query") (BS8.intercalate " " (map TE.encodeUtf8 sensors))

formatDuration :: Duration -> T.Text
formatDuration (Duration secs)
  | secs >= 24 * 3600 = T.pack (show $ secs / (24 * 3600)) <> "d"
  | secs >= 3600 = T.pack (show $ secs / 3600) <> "h"
  | secs >= 60 = T.pack (show $ secs / 60) <> "m"
  | secs >= 1 = T.pack (show secs) <> "s"
  | otherwise = T.pack (show $ secs * 1000) <> "ms"

parseDuration :: T.Text -> Either T.Text Duration
parseDuration inp = case T.span (\c -> C.isDigit c || c == '.' || c == '_') inp of
  ("", _) -> Left $ "Invalid duration format: empty string"
  (numPart, T.strip -> rest) ->
    case readEither (T.unpack numPart) of
      Left err -> Left $ "Invalid duration (bare seconds, or real number with suffix ms/s/m/h/d expected): " <> T.pack err
      Right num ->
        if T.null rest
          then Right $ Duration num
          else case T.toLower rest of
            "ms" -> Right $ Duration (num / 1000)
            "s" -> Right $ Duration num
            "m" -> Right $ Duration (num * 60)
            "h" -> Right $ Duration (num * 3600)
            "d" -> Right $ Duration (num * 86400)
            _ -> Left $ "Invalid duration suffix: expected no suffix (treated as second), or one of ms/s/m/h/d, but got: " <> rest

espresenseTopicFilters :: ESPresenseConfig -> [TopicFilter]
espresenseTopicFilters ESPresenseConfig {..} =
  [ "espresense" <> "devices" <> TopicFilter device <> wildOne
  | device <- devices
  ]
    <> [ "espresense" <> "rooms" <> TopicFilter room.name <> key
       | room <- sensors
       , key <- ["status", "telemetry"]
       ]

type ESPSensorName = T.Text

type ESPDeviceId = T.Text

data RawESPStatus = RawESPStatus
  { mac :: {-# UNPACK #-} !T.Text
  , id :: {-# UNPACK #-} !T.Text
  , name :: {-# UNPACK #-} !T.Text
  , rssi :: {-# UNPACK #-} !Float
  , rssiVar :: {-# UNPACK #-} !Float
  , distance :: {-# UNPACK #-} !Float
  , var :: {-# UNPACK #-} !Float
  , int :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ESPStatus = ESPStatus
  { timestamp :: {-# UNPACK #-} !UTCTime
  , sensor :: {-# UNPACK #-} !T.Text
  , mac :: {-# UNPACK #-} !T.Text
  , id :: {-# UNPACK #-} !T.Text
  , name :: {-# UNPACK #-} !T.Text
  , rssi :: {-# UNPACK #-} !Float
  , rssiVar :: {-# UNPACK #-} !Float
  , distance :: {-# UNPACK #-} !Float
  , var :: {-# UNPACK #-} !Float
  , int :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ESPSensorState = ESPSensorState
  { timestamp :: {-# UNPACK #-} !UTCTime
  , distance :: {-# UNPACK #-} !Float
  , variance :: {-# UNPACK #-} !Float
  , interval :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)

parseRawESPStatus ::
  MqttMessage ->
  ParseResult String (T.Text, RawESPStatus)
parseRawESPStatus msg =
  case stripPrefix "espresense/devices" msg.topic of
    Nothing -> Skipped
    Just (Topic rest) ->
      case T.splitOn "/" rest of
        [_, sensor] ->
          case A.eitherDecode' $ LBS.fromStrict msg.payload of
            Left err -> ParseFailure err
            Right stt -> ParseSuccess (sensor, stt)
        _ -> Skipped

buildESPStatus :: UTCTime -> T.Text -> RawESPStatus -> ESPStatus
buildESPStatus timestamp sensor stt =
  ESPStatus
    { timestamp
    , sensor
    , mac = stt.mac
    , id = stt.id
    , name = stt.name
    , rssi = stt.rssi
    , rssiVar = stt.rssiVar
    , distance = stt.distance
    , var = stt.var
    , int = stt.int
    }

parseESPStatusS ::
  (Time cl ~ UTCTime) =>
  ClSF (Eff es) cl MqttMessage (ParseResult String ESPStatus)
parseESPStatusS = proc msg -> do
  TimeInfo {..} <- timeInfo -< ()
  returnA -< uncurry (buildESPStatus absolute) <$> parseRawESPStatus msg

type OccupancyList = HashMap ESPDeviceId ESPStatus

isVacant :: OccupancyList -> Bool
isVacant = HM.null

occupants :: OccupancyList -> [ESPDeviceId]
occupants = HM.keys

numOccupants :: OccupancyList -> Int
numOccupants = HM.size

occupancyListS ::
  (Clock (Eff es) cl, Time cl ~ UTCTime) =>
  ESPSensor ->
  ClSF (Eff es) cl (Maybe ESPStatus) OccupancyList
occupancyListS room = feedback HM.empty $ proc (event, residents) -> do
  TimeInfo {..} <- timeInfo -< ()
  let residents' =
        HM.filter
          (\stt -> absolute `diffTime` stt.timestamp < room.timeout.seconds)
          residents

  case event of
    Nothing -> returnA -< (residents', residents')
    Just stt -> returnA -< (HM.insert stt.id stt residents', residents')

aggregateESPStatus ::
  BehaviourF (Eff es) UTCTime (Maybe ESPStatus) (HashMap (ESPSensorName, ESPDeviceId) ESPSensorState)
aggregateESPStatus = feedback HM.empty $ arr \(!mest, !prev) ->
  case mest of
    Nothing -> (prev, prev)
    Just stt ->
      let ssst =
            ESPSensorState
              { timestamp = stt.timestamp
              , distance = stt.distance
              , variance = stt.var
              , interval = stt.int
              }
          !new = HM.insert (stt.sensor, stt.id) ssst prev
       in (new, new)

instance ToMackerelMetrics ESPStatus where
  toMetrics stt =
    [ MackerelEntry
        { name = "espresense.distance." <> stt.sensor
        , time = stt.timestamp
        , value = A.Number (realToFrac stt.distance)
        }
    , MackerelEntry
        { name = "espresense.variance." <> stt.sensor
        , time = stt.timestamp
        , value = A.Number (realToFrac stt.var)
        }
    ]
