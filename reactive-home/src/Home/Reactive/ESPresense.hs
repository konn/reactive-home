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
  ESPresenseSnapshot (..),
  ESPresensePatch,
  ESPresenseDelta (..),
  espresenseDeltaS,
  aggregateESPresenseDeltaS,
  espresenseSnapshotS,
  ESPSensor (..),
  Duration (..),
  millis,
  seconds,
  minutes,
  hours,
  days,
  Heartbeated (..),
  ESPresenseConfig (..),
  Room (..),
  RoomSensor (..),
  DeviceStatus (..),
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
) where

import Control.Applicative (empty, (<|>))
import Control.Lens ((&), (.~))
import Control.Monad (unless, void)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Coerce (coerce)
import Data.DList.DNonEmpty qualified as DLNE
import Data.Foldable (find)
import Data.Generics.Labels ()
import Data.HashMap.Monoidal qualified as MHM
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (..))
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Network.Mqtt (Mqtt, QoS (..), defaultPublishOptions, publish)
import Effectful.Reader.Static (Reader)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.App.Types (ParseResult (..))
import Home.Reactive.MQTT
import Home.Reactive.Metrics.Mackerel
import Home.Reactive.Utils (
  MovingAverageConfig (..),
  effReaderS,
  movingAverageS,
 )
import Network.Mqtt.Types.Topic (stripPrefix)
import Text.Read (readEither)
import Toml hiding (first, map)
import Validation (Validation (..))

data ESPresenseSnapshot = ESPresenseSnapshot
  { sensors :: HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState)
  , -- , distanceAverages :: HashMap ESPSensorName (HashMap ESPDeviceId (Maybe Float))
    rooms :: HashMap T.Text [DeviceStatus]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type ESPresensePatch k v = HashMap k (Maybe v)

data ESPresenseDelta = ESPresenseDelta
  { sensors :: HashMap ESPSensorName (ESPresensePatch ESPDeviceId ESPSensorState)
  , rooms :: HashMap T.Text (ESPresensePatch ESPDeviceId DeviceStatus)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

nullESPresenseDelta :: ESPresenseDelta -> Bool
nullESPresenseDelta delta =
  HM.null delta.sensors && HM.null delta.rooms

data Heartbeated a = Heartbeat | Event !a
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

espresenseDeltaS ::
  ( Time cl ~ UTCTime
  , Reader ESPresenseConfig :> es
  ) =>
  ClSF (Eff es) cl (Heartbeated ESPStatus) (Maybe ESPresenseDelta)
espresenseDeltaS = effReaderS @ESPresenseConfig proc (evt, cfg) -> do
  sensors <-
    fmap (HM.filter (not . HM.null)) (parallely sensorStatusDeltaS)
      -<
        HM.fromList
          [ (sensor.name, (sensor, evt))
          | sensor <- cfg.sensors
          ]

  rooms <-
    fmap (HM.filter (not . HM.null)) (parallely roomPresenceDeltaS)
      -<
        HM.map (,evt) cfg.rooms

  let !delta = ESPresenseDelta {..}
  returnA -< if nullESPresenseDelta delta then Nothing else Just delta

aggregateESPresenseDeltaS ::
  (Reader ESPresenseConfig :> es) =>
  ClSF (Eff es) cl (Maybe ESPresenseDelta) ESPresenseSnapshot
aggregateESPresenseDeltaS =
  effReaderS @ESPresenseConfig $
    feedback emptyESPresenseState proc ((mdelta, cfg), prev) -> do
      let !new = maybe prev (`applyESPresenseDelta` prev) mdelta
          !snapshot = toESPresenseSnapshot cfg new
      returnA -< (snapshot, new)

espresenseSnapshotS ::
  ( Time cl ~ UTCTime
  , Reader ESPresenseConfig :> es
  ) =>
  ClSF (Eff es) cl (Heartbeated ESPStatus) ESPresenseSnapshot
espresenseSnapshotS = espresenseDeltaS >>> aggregateESPresenseDeltaS

expireSensorSnapshot ::
  ESPresenseConfig ->
  UTCTime ->
  HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState) ->
  HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState)
expireSensorSnapshot cfg now =
  HM.mapMaybeWithKey \sensorName devices ->
    let !freshDevices =
          HM.filter
            (\sensorState -> now `diffTime` sensorState.timestamp < sensorTimeout cfg sensorName)
            devices
     in if HM.null freshDevices then Nothing else Just freshDevices

data ESPSensor = ESPSensor
  { name :: !ESPSensorName
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
  name <- ESPSensorName <$> Toml.text "name" .= (.name.raw)
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
  { devices :: ![ESPDeviceId]
  , sensors :: ![ESPSensor]
  , rooms :: !(HashMap T.Text Room)
  }
  deriving (Show, Eq, Ord, Generic)

instance HasCodec ESPresenseConfig where
  hasCodec = Toml.table espresenseConfigCodec

instance HasItemCodec ESPresenseConfig where
  hasItemCodec = Right espresenseConfigCodec

data RoomSensor = RoomSensor
  { sensor :: !ESPSensorName
  , distance :: !Float
  }
  deriving (Show, Eq, Ord, Generic)

roomSensorCodec :: TomlCodec RoomSensor
roomSensorCodec = do
  sensor <- ESPSensorName <$> Toml.text "sensor" .= (.sensor.raw)
  distance <- numberFloat "distance" .= (.distance)
  pure RoomSensor {..}

data Room = Room
  { timeout :: !Duration
  , sensors :: ![RoomSensor]
  }
  deriving (Show, Eq, Ord, Generic)

data SensorParams = SensorParams
  { threshold :: !Float
  , window :: !Int
  , timeout :: !Duration
  , distance :: !Float
  , timestamp :: !UTCTime
  , sensor :: !ESPSensorName
  , device :: !ESPDeviceId
  }
  deriving (Show, Eq, Ord, Generic)

data DistanceAverageParams = DistanceAverageParams
  { window :: !Int
  , timeout :: !Duration
  , distance :: !Float
  }
  deriving (Show, Eq, Ord, Generic)

distanceAverageS ::
  (Time cl ~ UTCTime) =>
  ClSF (Eff es) cl DistanceAverageParams (Maybe Float)
distanceAverageS = proc DistanceAverageParams {..} -> do
  movingAverageS
    -<
      ( MovingAverageConfig
          { window = window
          , timeout = Just timeout.seconds
          }
      , distance
      )

sensorPresenceS ::
  (Time cl ~ UTCTime) =>
  ClSF (Eff es) cl SensorParams Bool
sensorPresenceS = proc SensorParams {..} -> do
  TimeInfo {..} <- timeInfo -< ()
  avg <-
    distanceAverageS
      -<
        DistanceAverageParams
          { window = window
          , timeout = timeout
          , distance = distance
          }
  let !fresh = absolute `diffTime` timestamp < timeout.seconds
      !present = maybe False (<= threshold) avg && fresh
  returnA -< present

data DeviceStatus = DeviceStatus
  { device :: !ESPDeviceId
  , seenBy :: NonEmpty (ESPSensorName, UTCTime)
  , lastSeen :: !UTCTime
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

roomPresenceDeltaS ::
  (Time cl ~ UTCTime, Reader ESPresenseConfig :> es) =>
  ClSF (Eff es) cl (Room, Heartbeated ESPStatus) (ESPresensePatch ESPDeviceId DeviceStatus)
roomPresenceDeltaS = effReaderS @ESPresenseConfig $
  feedback HM.empty proc (((room, evt), cfg), prevOccupants) -> do
    TimeInfo {..} <- timeInfo -< ()
    let !occupants =
          HM.filter
            (\stt -> absolute `diffTime` stt < room.timeout.seconds)
            $ HM.filterWithKey
              ( \(sensorName, _) stt ->
                  absolute `diffTime` stt < sensorTimeout cfg sensorName
              )
              prevOccupants
    occupants' <- case evt of
      Event stt
        | Just sensor <- find (\s -> s.name == stt.sensor) cfg.sensors
        , Just sensorCfg <- find (\s -> s.sensor == stt.sensor) room.sensors -> do
            let ps =
                  SensorParams
                    { threshold = sensorCfg.distance
                    , window = fromMaybe 5 sensor.window
                    , timeout = sensor.timeout
                    , distance = stt.distance
                    , timestamp = stt.timestamp
                    , sensor = stt.sensor
                    , device = stt.id
                    }
            present <- sensorPresenceS -< ps
            if present
              then returnA -< HM.insert (sensor.name, stt.id) stt.timestamp occupants
              else returnA -< HM.delete (sensor.name, stt.id) occupants
      _ -> returnA -< occupants
    let !delta = diffPatch (toOccupancyMap prevOccupants) (toOccupancyMap occupants')
    returnA -< (delta, occupants')

sensorTimeout :: ESPresenseConfig -> ESPSensorName -> Diff UTCTime
sensorTimeout cfg sensorName =
  maybe 0 (.timeout.seconds) $ find (\sensor -> sensor.name == sensorName) cfg.sensors

toOccupancyMap :: HashMap (ESPSensorName, ESPDeviceId) UTCTime -> HashMap ESPDeviceId DeviceStatus
toOccupancyMap =
  HM.fromList
    . map
      ( \(device, (seenBy, Max lastSeen)) ->
          (device, DeviceStatus {device, seenBy = DLNE.toNonEmpty seenBy, lastSeen})
      )
    . MHM.toList
    . HM.foldMapWithKey
      ( \(sensor, device) lastSeen ->
          MHM.singleton
            device
            (DLNE.singleton (sensor, lastSeen), Max lastSeen)
      )

roomRuleCodec :: TomlCodec Room
roomRuleCodec = do
  timeout <- hasCodec "timeout" .= (.timeout)
  sensors <- Toml.list roomSensorCodec "sensors" .= (.sensors)
  pure Room {..}

espresenseConfigCodec :: TomlCodec ESPresenseConfig
espresenseConfigCodec = validateConfigCodec baseCodec
  where
    baseCodec :: TomlCodec ESPresenseConfig
    baseCodec = do
      devices <- coerce <$> Toml.arrayOf Toml._Text "devices" .= map (.raw) . (.devices)
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
      if hasObsoleteRoomTables roomToml
        then empty
        else (\room -> (roomName, room)) <$> codecRead roomRuleCodec roomToml

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
            case firstInvalidRoomSensors cfg of
              Nothing -> pure cfg
              Just _ -> empty
    , codecWrite = codecWrite codec
    }

hasObsoleteRoomTables :: TOML -> Bool
hasObsoleteRoomTables toml =
  Prelude.any
    ( \case
        Toml.Piece "entry" :|| _ -> True
        Toml.Piece "leave" :|| _ -> True
        Toml.Piece "return" :|| _ -> True
        _ -> False
    )
    (fst <$> Toml.toList toml.tomlTables)

firstInvalidRoomSensors :: ESPresenseConfig -> Maybe (T.Text, [ESPSensorName])
firstInvalidRoomSensors cfg =
  find (not . null . snd) $
    [ (roomName, filter (`notElem` sensorNames) (map (.sensor) room.sensors))
    | (roomName, room) <- HM.toList cfg.rooms
    ]
  where
    sensorNames = map (.name) cfg.sensors

newtype Duration = Duration {seconds :: Diff UTCTime}
  deriving (Eq, Ord, Generic)
  deriving newtype (Hashable)

instance ToJSON Duration where
  toEncoding (Duration secs) = A.toEncoding (formatDuration (Duration secs))

instance FromJSON Duration where
  parseJSON v = do
    txt <- A.parseJSON v
    either (fail . T.unpack) pure (parseDuration txt)

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
  let setTopic k = "espresense" <> "rooms" <> Topic room.name.raw <> Topic k <> "set"
  pub (setTopic "max_distance") (BS8.pack $ show room.max_distance)
  pub (setTopic "skip_distance") (BS8.pack $ show room.skip_distance)
  pub (setTopic "skip_ms") (BS8.pack $ show room.skip_ms)

  unless (null sensors) $ do
    void $ pub (setTopic "include") (BS8.intercalate " " (map (TE.encodeUtf8 . (.raw)) sensors))
    void $ pub (setTopic "query") (BS8.intercalate " " (map (TE.encodeUtf8 . (.raw)) sensors))

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
  [ "espresense" <> "devices" <> TopicFilter device.raw <> wildOne
  | device <- devices
  ]
    <> [ "espresense" <> "rooms" <> TopicFilter room.name.raw <> key
       | room <- sensors
       , key <- ["status", "telemetry"]
       ]

newtype ESPSensorName = ESPSensorName {raw :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype
    ( Show
    , FromJSON
    , ToJSON
    , FromJSONKey
    , ToJSONKey
    , IsString
    , HasCodec
    , HasItemCodec
    , Hashable
    )

newtype ESPDeviceId = ESPDeviceId {raw :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype
    ( Show
    , FromJSON
    , ToJSON
    , FromJSONKey
    , ToJSONKey
    , IsString
    , HasCodec
    , HasItemCodec
    , Hashable
    )

data RawESPStatus = RawESPStatus
  { mac :: {-# UNPACK #-} !T.Text
  , id :: {-# UNPACK #-} !ESPDeviceId
  , name :: {-# UNPACK #-} !T.Text
  , rssi :: {-# UNPACK #-} !Float
  , distance :: {-# UNPACK #-} !Float
  , var :: {-# UNPACK #-} !Float
  , int :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ESPStatus = ESPStatus
  { timestamp :: {-# UNPACK #-} !UTCTime
  , sensor :: {-# UNPACK #-} !ESPSensorName
  , mac :: {-# UNPACK #-} !T.Text
  , id :: {-# UNPACK #-} !ESPDeviceId
  , name :: {-# UNPACK #-} !T.Text
  , rssi :: {-# UNPACK #-} !Float
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
  deriving anyclass (FromJSON, ToJSON)

parseRawESPStatus ::
  MqttMessage ->
  ParseResult String (ESPSensorName, RawESPStatus)
parseRawESPStatus msg =
  case stripPrefix "espresense/devices" msg.topic of
    Nothing -> Skipped
    Just (Topic rest) ->
      case T.splitOn "/" rest of
        [_, sensor] ->
          case A.eitherDecode' $ LBS.fromStrict msg.payload of
            Left err -> ParseFailure err
            Right stt -> ParseSuccess (ESPSensorName sensor, stt)
        _ -> Skipped

buildESPStatus :: UTCTime -> ESPSensorName -> RawESPStatus -> ESPStatus
buildESPStatus timestamp sensor stt =
  ESPStatus
    { timestamp
    , sensor
    , mac = stt.mac
    , id = stt.id
    , name = stt.name
    , rssi = stt.rssi
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

aggregateESPStatus ::
  (Reader ESPresenseConfig :> es) =>
  BehaviourF
    (Eff es)
    UTCTime
    (Maybe ESPStatus)
    (HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState))
aggregateESPStatus =
  effReaderS @ESPresenseConfig $
    feedback HM.empty proc ((!mest, cfg), !prev) -> do
      TimeInfo {..} <- timeInfo -< ()
      let !new = expireSensorSnapshot cfg absolute $ case mest of
            Nothing -> prev
            Just stt -> insertSensorState stt prev
      returnA -< (new, new)

sensorStatusDeltaS ::
  (Time cl ~ UTCTime) =>
  ClSF (Eff es) cl (ESPSensor, Heartbeated ESPStatus) (ESPresensePatch ESPDeviceId ESPSensorState)
sensorStatusDeltaS = feedback HM.empty proc ((sensor, evt), prev) -> do
  TimeInfo {..} <- timeInfo -< ()
  let !fresh =
        HM.filter
          (\sensorState -> absolute `diffTime` sensorState.timestamp < sensor.timeout.seconds)
          prev
      !new = case evt of
        Event stt
          | stt.sensor == sensor.name ->
              HM.insert stt.id (sensorStateFromStatus stt) fresh
        _ -> fresh
      !delta = diffPatch prev new
  returnA -< (delta, new)

insertSensorState ::
  ESPStatus ->
  HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState) ->
  HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState)
insertSensorState stt =
  HM.insertWith HM.union stt.sensor (HM.singleton stt.id (sensorStateFromStatus stt))

sensorStateFromStatus :: ESPStatus -> ESPSensorState
sensorStateFromStatus stt =
  ESPSensorState
    { timestamp = stt.timestamp
    , distance = stt.distance
    , variance = stt.var
    , interval = stt.int
    }

data ESPresenseState = ESPresenseState
  { sensors :: HashMap ESPSensorName (HashMap ESPDeviceId ESPSensorState)
  , rooms :: HashMap T.Text (HashMap ESPDeviceId DeviceStatus)
  }
  deriving (Show, Eq, Ord, Generic)

emptyESPresenseState :: ESPresenseState
emptyESPresenseState =
  ESPresenseState
    { sensors = HM.empty
    , rooms = HM.empty
    }

applyESPresenseDelta :: ESPresenseDelta -> ESPresenseState -> ESPresenseState
applyESPresenseDelta delta prev =
  ESPresenseState
    { sensors =
        HM.filter (not . HM.null) $
          applyNestedPatch delta.sensors prev.sensors
    , rooms =
        HM.filter (not . HM.null) $
          applyNestedPatch delta.rooms prev.rooms
    }

toESPresenseSnapshot :: ESPresenseConfig -> ESPresenseState -> ESPresenseSnapshot
toESPresenseSnapshot cfg state =
  ESPresenseSnapshot
    { sensors = state.sensors
    , rooms = HM.map HM.elems $ HM.union state.rooms emptyRooms
    }
  where
    emptyRooms = HM.map (const HM.empty) cfg.rooms

applyNestedPatch ::
  (Hashable k, Hashable k') =>
  HashMap k (ESPresensePatch k' v) ->
  HashMap k (HashMap k' v) ->
  HashMap k (HashMap k' v)
applyNestedPatch patch prev =
  HM.foldlWithKey'
    (\acc key nestedPatch -> HM.insert key (applyPatch nestedPatch (HM.lookupDefault HM.empty key acc)) acc)
    prev
    patch

applyPatch ::
  (Hashable k) =>
  ESPresensePatch k v ->
  HashMap k v ->
  HashMap k v
applyPatch patch prev =
  HM.foldlWithKey'
    ( \acc key -> \case
        Nothing -> HM.delete key acc
        Just value -> HM.insert key value acc
    )
    prev
    patch

diffPatch ::
  (Hashable k, Eq v) =>
  HashMap k v ->
  HashMap k v ->
  ESPresensePatch k v
diffPatch old new =
  HM.foldMapWithKey
    (\key _ -> if HM.member key new then HM.empty else HM.singleton key Nothing)
    old
    <> HM.foldMapWithKey
      (\key value -> if HM.lookup key old == Just value then HM.empty else HM.singleton key (Just value))
      new

instance ToMackerelMetrics ESPStatus where
  toMetrics stt =
    [ MackerelEntry
        { name = "espresense.distance." <> stt.sensor.raw
        , time = stt.timestamp
        , value = A.Number (realToFrac stt.distance)
        }
    , MackerelEntry
        { name = "espresense.variance." <> stt.sensor.raw
        , time = stt.timestamp
        , value = A.Number (realToFrac stt.var)
        }
    ]
