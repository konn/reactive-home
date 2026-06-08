{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Home.Reactive.ESPresenseTest (test_tomlParsing, test_roomAbsence, test_deltas) where

import Control.Monad.Trans.Reader (runReaderT)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static (Reader, runReader)
import FRP.Rhine (
  ClSF,
  Clock (..),
  Result (..),
  TimeInfo (..),
  stepAutomaton,
 )
import Home.Reactive.ESPresense (
  DeviceStatus (..),
  ESPSensor (..),
  ESPSensorName,
  ESPSensorState (..),
  ESPStatus (..),
  ESPresenseConfig (..),
  ESPresenseDelta (..),
  ESPresenseSnapshot (..),
  Heartbeated (..),
  Room (..),
  RoomSensor (..),
  espresenseConfigCodec,
  espresenseDeltaS,
  espresenseSnapshotS,
  minutes,
  seconds,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Toml (decodeExact)

data TestClock = TestClock

instance Clock (Eff es) TestClock where
  type Time TestClock = UTCTime
  type Tag TestClock = ()

  initClock TestClock =
    error "TestClock is stepped manually in ESPresense tests"

test_tomlParsing :: TestTree
test_tomlParsing =
  testGroup
    "TOML parsing"
    [ testCase "uses ESPRoom defaults only when optional keys are absent" $
        decodeExact espresenseConfigCodec minimalRoomToml
          @?= Right
            ESPresenseConfig
              { devices = []
              , sensors =
                  [ ESPSensor
                      { name = "office"
                      , max_distance = 16
                      , skip_distance = 0.5
                      , skip_ms = 5000
                      , timeout = seconds 5
                      , window = Nothing
                      }
                  ]
              , rooms = HM.empty
              }
    , testCase "keeps present ESPRoom values instead of replacing them with defaults" $
        decodeExact espresenseConfigCodec explicitRoomToml
          @?= Right
            ESPresenseConfig
              { devices = []
              , sensors =
                  [ ESPSensor
                      { name = "office"
                      , max_distance = 3.25
                      , skip_distance = 1.25
                      , skip_ms = 1234
                      , timeout = seconds 5
                      , window = Nothing
                      }
                  ]
              , rooms = HM.empty
              }
    , testCase "parses real example TOML correctly" $
        decodeExact espresenseConfigCodec realExampleToml
          @?= Right
            ESPresenseConfig
              { devices = ["watch:"]
              , sensors =
                  [ ESPSensor
                      { name = "room"
                      , max_distance = 8
                      , skip_distance = 0.5
                      , skip_ms = 5000
                      , timeout = seconds 5
                      , window = Nothing
                      }
                  , ESPSensor
                      { name = "bedroom"
                      , max_distance = 8
                      , skip_distance = 0.5
                      , skip_ms = 5000
                      , timeout = seconds 5.5
                      , window = Nothing
                      }
                  ]
              , rooms = HM.empty
              }
    , testCase "does not hide invalid present ESPRoom values behind defaults" $
        case decodeExact espresenseConfigCodec invalidRoomToml of
          Left _ -> pure ()
          Right cfg -> assertFailure $ "expected TOML decode failure, got: " <> show cfg
    , testCase "parses rooms with sensor distance limits" $
        decodeExact espresenseConfigCodec roomsToml
          @?= Right
            ESPresenseConfig
              { devices = ["watch:"]
              , sensors =
                  [ ESPSensor
                      { name = "entrance"
                      , max_distance = 16
                      , skip_distance = 0.5
                      , skip_ms = 5000
                      , timeout = seconds 5
                      , window = Nothing
                      }
                  , ESPSensor
                      { name = "bedroom"
                      , max_distance = 16
                      , skip_distance = 0.5
                      , skip_ms = 5000
                      , timeout = seconds 5
                      , window = Nothing
                      }
                  ]
              , rooms =
                  HM.fromList
                    [
                      ( "home"
                      , Room
                          { timeout = minutes 3
                          , sensors =
                              [ RoomSensor {sensor = "entrance", distance = 6.5}
                              , RoomSensor {sensor = "bedroom", distance = 5}
                              ]
                          }
                      )
                    ]
              }
    , testCase "rejects unknown room sensors" $
        case decodeExact espresenseConfigCodec invalidRoomSensorToml of
          Left _ -> pure ()
          Right cfg -> assertFailure $ "expected TOML decode failure, got: " <> show cfg
    , testCase "rejects obsolete leave/entry room config" $
        case decodeExact espresenseConfigCodec obsoleteRoomConditionsToml of
          Left _ -> pure ()
          Right cfg -> assertFailure $ "expected TOML decode failure, got: " <> show cfg
    ]

test_roomAbsence :: TestTree
test_roomAbsence =
  testGroup
    "room absence"
    [ testCase "heartbeat reports configured rooms empty after ESP sensor timeout" $ do
        let snapshots =
              runSnapshotInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime 1 baseTime) Heartbeat
                , TestInput (addUTCTime 3 baseTime) Heartbeat
                ]
        length . (HM.! "home") . (.rooms) <$> snapshots @?= [1, 1, 0]
    , testCase "heartbeat keeps room occupied before ESP sensor timeout" $ do
        let snapshots =
              runSnapshotInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "bedroom" 1)
                , TestInput (addUTCTime 1 baseTime) Heartbeat
                ]
        length ((last snapshots).rooms HM.! "home") @?= 1
    , testCase "heartbeat removes stale sensor snapshots after ESP sensor timeout" $ do
        let snapshots =
              runSnapshotInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime 1 baseTime) Heartbeat
                , TestInput (addUTCTime 3 baseTime) Heartbeat
                ]
        sensorDeviceCount "entrance" <$> snapshots @?= [1, 1, 0]
    ]

test_deltas :: TestTree
test_deltas =
  testGroup
    "deltas"
    [ testCase "event emits sensor and room upserts" $ do
        let deltas =
              runDeltaInputs
                absenceConfig
                [TestInput baseTime (Event $ statusAt baseTime "entrance" 1)]
        deltas
          @?= [ Just
                  ESPresenseDelta
                    { sensors =
                        HM.fromList
                          [ ("entrance", HM.fromList [("watch:", Just $ sensorStateAt baseTime 1)])
                          ]
                    , rooms =
                        HM.fromList
                          [ ("home", HM.fromList [("watch:", Just $ deviceStatus [("entrance", baseTime)])])
                          ]
                    }
              ]
    , testCase "heartbeat before timeout emits no deltas" $ do
        let deltas =
              runDeltaInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime 1 baseTime) Heartbeat
                ]
        last deltas
          @?= Nothing
    , testCase "heartbeat after sensor timeout emits sensor and room deletes" $ do
        let deltas =
              runDeltaInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime 3 baseTime) Heartbeat
                ]
        last deltas
          @?= Just
            ESPresenseDelta
              { sensors = HM.fromList [("entrance", HM.fromList [("watch:", Nothing)])]
              , rooms = HM.fromList [("home", HM.fromList [("watch:", Nothing)])]
              }
    , testCase "expiring one of two sensors updates room status before final delete" $ do
        let bedroomTime = addUTCTime 0.5 baseTime
            deltas =
              runDeltaInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput bedroomTime (Event $ statusAt bedroomTime "bedroom" 1)
                , TestInput (addUTCTime 2.25 baseTime) Heartbeat
                , TestInput (addUTCTime 3 baseTime) Heartbeat
                ]
        deltas
          @?= [ Just
                  ESPresenseDelta
                    { sensors = HM.fromList [("entrance", HM.fromList [("watch:", Just $ sensorStateAt baseTime 1)])]
                    , rooms = HM.fromList [("home", HM.fromList [("watch:", Just $ deviceStatus [("entrance", baseTime)])])]
                    }
              , Just
                  ESPresenseDelta
                    { sensors = HM.fromList [("bedroom", HM.fromList [("watch:", Just $ sensorStateAt bedroomTime 1)])]
                    , rooms =
                        HM.fromList
                          [
                            ( "home"
                            , HM.fromList
                                [ ("watch:", Just $ deviceStatus [("bedroom", bedroomTime), ("entrance", baseTime)])
                                ]
                            )
                          ]
                    }
              , Just
                  ESPresenseDelta
                    { sensors = HM.fromList [("entrance", HM.fromList [("watch:", Nothing)])]
                    , rooms = HM.fromList [("home", HM.fromList [("watch:", Just $ deviceStatus [("bedroom", bedroomTime)])])]
                    }
              , Just
                  ESPresenseDelta
                    { sensors = HM.fromList [("bedroom", HM.fromList [("watch:", Nothing)])]
                    , rooms = HM.fromList [("home", HM.fromList [("watch:", Nothing)])]
                    }
              ]
    , testCase "far distance removes room presence but keeps sensor state" $ do
        let farTime = addUTCTime 1 baseTime
            deltas =
              runDeltaInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput farTime (Event $ statusAt farTime "entrance" 4)
                ]
        last deltas
          @?= Just
            ESPresenseDelta
              { sensors = HM.fromList [("entrance", HM.fromList [("watch:", Just $ sensorStateAt farTime 4)])]
              , rooms = HM.fromList [("home", HM.fromList [("watch:", Nothing)])]
              }
    ]

data TestInput = TestInput
  { at :: !UTCTime
  , input :: !(Heartbeated ESPStatus)
  }

type SnapshotS =
  ClSF
    (Eff '[Reader ESPresenseConfig])
    TestClock
    (Heartbeated ESPStatus)
    ESPresenseSnapshot

type DeltaS =
  ClSF
    (Eff '[Reader ESPresenseConfig])
    TestClock
    (Heartbeated ESPStatus)
    (Maybe ESPresenseDelta)

runSnapshotInputs :: ESPresenseConfig -> [TestInput] -> [ESPresenseSnapshot]
runSnapshotInputs cfg inputs =
  runPureEff $ runReader cfg $ go espresenseSnapshotS Nothing inputs
  where
    go :: SnapshotS -> Maybe UTCTime -> [TestInput] -> Eff '[Reader ESPresenseConfig] [ESPresenseSnapshot]
    go _ _ [] = pure []
    go signal previous (TestInput {..} : rest) = do
      let timeInfo =
            TimeInfo
              { sinceLast = maybe 0 (realToFrac . (at `diffUTCTime`)) previous
              , sinceInit = realToFrac $ at `diffUTCTime` baseTime
              , absolute = at
              , tag = ()
              }
      Result signal' snapshot <- runReaderT (stepAutomaton signal input) timeInfo
      (snapshot :) <$> go signal' (Just at) rest

runDeltaInputs :: ESPresenseConfig -> [TestInput] -> [Maybe ESPresenseDelta]
runDeltaInputs cfg inputs =
  runPureEff $ runReader cfg $ go espresenseDeltaS Nothing inputs
  where
    go :: DeltaS -> Maybe UTCTime -> [TestInput] -> Eff '[Reader ESPresenseConfig] [Maybe ESPresenseDelta]
    go _ _ [] = pure []
    go signal previous (TestInput {..} : rest) = do
      let timeInfo =
            TimeInfo
              { sinceLast = maybe 0 (realToFrac . (at `diffUTCTime`)) previous
              , sinceInit = realToFrac $ at `diffUTCTime` baseTime
              , absolute = at
              , tag = ()
              }
      Result signal' delta <- runReaderT (stepAutomaton signal input) timeInfo
      (delta :) <$> go signal' (Just at) rest

sensorDeviceCount :: ESPSensorName -> ESPresenseSnapshot -> Int
sensorDeviceCount sensor snapshot =
  maybe 0 HM.size $ HM.lookup sensor snapshot.sensors

sensorStateAt :: UTCTime -> Float -> ESPSensorState
sensorStateAt timestamp distance =
  ESPSensorState
    { timestamp
    , distance
    , variance = 0.1
    , interval = 300
    }

deviceStatus :: [(ESPSensorName, UTCTime)] -> DeviceStatus
deviceStatus [] = error "deviceStatus test helper needs at least one sensor"
deviceStatus (seen : seenRest) =
  DeviceStatus
    { device = "watch:"
    , seenBy = seen :| seenRest
    , lastSeen = maximum (snd <$> (seen : seenRest))
    }

absenceConfig :: ESPresenseConfig
absenceConfig =
  ESPresenseConfig
    { devices = ["watch:"]
    , sensors =
        [ ESPSensor
            { name = "entrance"
            , max_distance = 16
            , skip_distance = 0.5
            , skip_ms = 5000
            , timeout = seconds 2
            , window = Just 1
            }
        , ESPSensor
            { name = "bedroom"
            , max_distance = 16
            , skip_distance = 0.5
            , skip_ms = 5000
            , timeout = seconds 2
            , window = Just 1
            }
        ]
    , rooms =
        HM.fromList
          [
            ( "home"
            , Room
                { timeout = minutes 3
                , sensors =
                    [ RoomSensor {sensor = "entrance", distance = 2}
                    , RoomSensor {sensor = "bedroom", distance = 2}
                    ]
                }
            )
          ]
    }

statusAt :: UTCTime -> ESPSensorName -> Float -> ESPStatus
statusAt timestamp sensor distance =
  ESPStatus
    { timestamp
    , sensor
    , mac = "5da2c2ab0a40"
    , id = "watch:"
    , name = "Watch"
    , rssi = -65
    , rssiVar = 1
    , distance
    , var = 0.1
    , int = 300
    }

baseTime :: UTCTime
baseTime = read "2026-06-08 03:53:40 UTC"

minimalRoomToml :: T.Text
minimalRoomToml =
  """
  devices = []

  [[sensors]]
  name = "office"
  """

explicitRoomToml :: T.Text
explicitRoomToml =
  """
  devices = []

  [[sensors]]
  name = "office"
  max_distance = 3.25
  skip_distance = 1.25
  skip_ms = 1234
  """

invalidRoomToml :: T.Text
invalidRoomToml =
  """
  devices = []

  [[sensors]]
  name = "office"
  max_distance = "far"
  """

realExampleToml :: T.Text
realExampleToml =
  """
  devices = ["watch:"]

  [[sensors]]
  name = "room"
  max_distance = 8
  skip_distance = 0.5
  skip_ms = 5000

  [[sensors]]
  name = "bedroom"
  max_distance = 8
  skip_distance = 0.5
  skip_ms = 5000
  timeout = "5.5s"
  """

roomsToml :: T.Text
roomsToml =
  """
  devices = ["watch:"]

  [[sensors]]
  name = "entrance"

  [[sensors]]
  name = "bedroom"

  [rooms.home]
  timeout = "3m"

  [[rooms.home.sensors]]
  sensor = "entrance"
  distance = 6.5

  [[rooms.home.sensors]]
  sensor = "bedroom"
  distance = 5
  """

invalidRoomSensorToml :: T.Text
invalidRoomSensorToml =
  """
  devices = ["watch:"]

  [[sensors]]
  name = "entrance"

  [rooms.home]
  timeout = "3m"

  [[rooms.home.sensors]]
  sensor = "bedroom"
  distance = 6.5
  """

obsoleteRoomConditionsToml :: T.Text
obsoleteRoomConditionsToml =
  """
  devices = ["watch:"]

  [[sensors]]
  name = "entrance"

  [rooms.home]
  timeout = "3m"

  [rooms.home.leave]
  conditions = [{ sensor = "entrance", device = "watch:", distance = 6.5 }]

  [rooms.home.entry]
  conditions = [{ sensor = "entrance", device = "watch:", distance = 5 }]
  """
