{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Home.Reactive.ESPresenseTest (test_tomlParsing, test_roomAbsence, test_deltas, test_unlockHeartbeat) where

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
import Home.Reactive.MQTT (MqttSnapshot (..))
import Home.Reactive.Unlock (
  ApproachCondition (..),
  DismissCondition (..),
  UnlockConfig (..),
  UnlockEvent (..),
  UnlockFeedback (..),
  UnlockStatus (..),
  unlockEventS,
  unlockFeedbackS,
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
    [ testCase "heartbeat keeps configured rooms occupied after ESP sensor timeout" $ do
        let snapshots =
              runSnapshotInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime 1 baseTime) Heartbeat
                , TestInput (addUTCTime 3 baseTime) Heartbeat
                ]
        length . (HM.! "home") . (.rooms) <$> snapshots @?= [1, 1, 1]
    , testCase "heartbeat reports configured rooms empty after room timeout" $ do
        let snapshots =
              runSnapshotInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime 1 baseTime) Heartbeat
                , TestInput (addUTCTime (3 * 60 + 1) baseTime) Heartbeat
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
    , testCase "interleaved far sensor readings do not clear another sensor's room presence" $ do
        let bedroom1 = addUTCTime 1 baseTime
            bedroom2 = addUTCTime 2 baseTime
            entrance1 = addUTCTime 3 baseTime
            entrance2 = addUTCTime 4 baseTime
            entrance3 = addUTCTime 5 baseTime
            bedroom3 = addUTCTime 6 baseTime
            snapshots =
              runSnapshotInputs
                windowedPresenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "bedroom" 1)
                , TestInput bedroom1 (Event $ statusAt bedroom1 "bedroom" 1)
                , TestInput bedroom2 (Event $ statusAt bedroom2 "bedroom" 1)
                , TestInput entrance1 (Event $ statusAt entrance1 "entrance" 8)
                , TestInput entrance2 (Event $ statusAt entrance2 "entrance" 8)
                , TestInput entrance3 (Event $ statusAt entrance3 "entrance" 8)
                , TestInput bedroom3 (Event $ statusAt bedroom3 "bedroom" 1)
                ]
            finalSnapshot = last snapshots
        length (finalSnapshot.rooms HM.! "home") @?= 1
        sensorDeviceCount "bedroom" finalSnapshot @?= 1
        sensorDeviceCount "entrance" finalSnapshot @?= 1
    , testCase "room presence bridges ESP sensor report gaps shorter than room timeout" $ do
        let gapEnd = addUTCTime 31 baseTime
            snapshots =
              runSnapshotInputs
                windowedPresenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "bedroom" 1)
                , TestInput (addUTCTime 1 baseTime) (Event $ statusAt (addUTCTime 1 baseTime) "bedroom" 1)
                , TestInput (addUTCTime 2 baseTime) (Event $ statusAt (addUTCTime 2 baseTime) "bedroom" 1)
                , TestInput gapEnd Heartbeat
                , TestInput (addUTCTime 32 baseTime) (Event $ statusAt (addUTCTime 32 baseTime) "entrance" 1.5)
                ]
        length . (HM.! "home") . (.rooms) <$> snapshots @?= [0, 0, 1, 1, 1]
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
    , testCase "heartbeat after sensor timeout emits only sensor deletes" $ do
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
              , rooms = HM.empty
              }
    , testCase "heartbeat after room timeout emits room deletes" $ do
        let deltas =
              runDeltaInputs
                absenceConfig
                [ TestInput baseTime (Event $ statusAt baseTime "entrance" 1)
                , TestInput (addUTCTime (3 * 60 + 1) baseTime) Heartbeat
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
                , TestInput (addUTCTime (3 * 60 + 1) baseTime) Heartbeat
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
                    , rooms = HM.empty
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

test_unlockHeartbeat :: TestTree
test_unlockHeartbeat =
  testGroup
    "unlock heartbeat"
    [ testCase "unchanged vacant snapshots advance waiting state to allow later unlock" $ do
        let vacantTime = addUTCTime 1 baseTime
            readyTime = addUTCTime 5 baseTime
            returnTime = addUTCTime 6 baseTime
            occupiedTime = addUTCTime 7 baseTime
            snapshots =
              [ TestSnapshot baseTime (occupiedSnapshot baseTime)
              , TestSnapshot vacantTime vacantSnapshot
              , TestSnapshot readyTime vacantSnapshot
              , TestSnapshot returnTime (occupiedSnapshot returnTime)
              , TestSnapshot occupiedTime (partialRoomOccupiedSnapshot occupiedTime)
              ]
        runUnlockInputs unlockConfig snapshots @?= [Nothing, Nothing, Nothing, Just Unlock, Nothing]
    , testCase "reports unlock feedback state on heartbeat snapshots" $ do
        let vacantTime = addUTCTime 1 baseTime
            readyTime = addUTCTime 5 baseTime
            snapshots =
              [ TestSnapshot baseTime (occupiedSnapshot baseTime)
              , TestSnapshot vacantTime vacantSnapshot
              , TestSnapshot readyTime vacantSnapshot
              ]
        (.status) . fst <$> runUnlockFeedbackInputs unlockConfig snapshots @?= [Occupied, Waiting, Vacant]
    , testCase "approach after two-sensor room re-occupies still emits unlock" $ do
        let vacantTime = addUTCTime 1 baseTime
            readyTime = addUTCTime (3 * 60 + 1) baseTime
            bedroomReturnTime = addUTCTime (3 * 60 + 2) baseTime
            approachTime = addUTCTime (3 * 60 + 3) baseTime
            occupiedTime = addUTCTime (3 * 60 + 4) baseTime
            partialOccupiedTime = addUTCTime (3 * 60 + 5) baseTime
            snapshots =
              [ TestSnapshot baseTime (exampleOccupiedSnapshot baseTime 6.0)
              , TestSnapshot vacantTime vacantSnapshot
              , TestSnapshot readyTime vacantSnapshot
              , TestSnapshot bedroomReturnTime (exampleBedroomSnapshot bedroomReturnTime 4.0)
              , TestSnapshot approachTime (exampleTwoSensorSnapshot bedroomReturnTime approachTime)
              , TestSnapshot occupiedTime (exampleTwoSensorSnapshot bedroomReturnTime occupiedTime)
              , TestSnapshot partialOccupiedTime (examplePartialRoomOccupiedSnapshot partialOccupiedTime)
              ]
        runUnlockInputs exampleUnlockConfig snapshots @?= [Nothing, Nothing, Nothing, Nothing, Just Unlock, Nothing, Nothing]
    , testCase "heartbeat-only vacancy permits first approach unlock" $ do
        let readyTime = addUTCTime (3 * 60 + 1) baseTime
            approachTime = addUTCTime (3 * 60 + 2) baseTime
            snapshots =
              [ TestSnapshot baseTime vacantSnapshot
              , TestSnapshot readyTime vacantSnapshot
              , TestSnapshot approachTime (exampleOccupiedSnapshot approachTime 4.5)
              ]
        runUnlockInputs exampleUnlockConfig snapshots @?= [Nothing, Nothing, Just Unlock]
    , testCase "inactive approach after ready does not block later approach unlock" $ do
        let inactiveTime = addUTCTime 1 baseTime
            vacantTime = addUTCTime 2 baseTime
            readyTime = addUTCTime (3 * 60 + 2) baseTime
            occupiedInactiveTime = addUTCTime (3 * 60 + 3) baseTime
            vacantAgainTime = addUTCTime (3 * 60 + 4) baseTime
            approachTime = addUTCTime (3 * 60 + 5) baseTime
            snapshots =
              [ TestSnapshot baseTime (exampleOccupiedSnapshot baseTime 4.5)
              , TestSnapshot inactiveTime (exampleOccupiedSnapshot inactiveTime 6.0)
              , TestSnapshot vacantTime vacantSnapshot
              , TestSnapshot readyTime vacantSnapshot
              , TestSnapshot occupiedInactiveTime (exampleOccupiedSnapshot occupiedInactiveTime 6.0)
              , TestSnapshot vacantAgainTime vacantSnapshot
              , TestSnapshot approachTime (exampleOccupiedSnapshot approachTime 4.5)
              ]
            feedbacks = runUnlockFeedbackInputs exampleUnlockConfig snapshots
        snd <$> feedbacks @?= [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just Unlock]
        (.status) . fst <$> feedbacks @?= [Occupied, Occupied, Waiting, Vacant, ReadyForUnlock, Vacant, Occupied]
    , testCase "far room presence after vacancy does not restart waiting" $ do
        let vacantTime = addUTCTime 1 baseTime
            readyTime = addUTCTime (3 * 60 + 1) baseTime
            farOccupiedTime = addUTCTime (3 * 60 + 2) baseTime
            farVacantTime = addUTCTime (3 * 60 + 3) baseTime
            heartbeatTime = addUTCTime (3 * 60 + 4) baseTime
            snapshots =
              [ TestSnapshot baseTime (exampleOccupiedSnapshot baseTime 6.0)
              , TestSnapshot vacantTime vacantSnapshot
              , TestSnapshot readyTime vacantSnapshot
              , TestSnapshot farOccupiedTime (exampleOccupiedSnapshot farOccupiedTime 6.0)
              , TestSnapshot farVacantTime (exampleSensorOnlySnapshot farVacantTime 6.0)
              , TestSnapshot heartbeatTime (exampleSensorOnlySnapshot farVacantTime 6.0)
              ]
            feedbacks = runUnlockFeedbackInputs exampleUnlockConfig snapshots
        snd <$> feedbacks @?= replicate 6 Nothing
        (.status) . fst <$> feedbacks @?= [Occupied, Waiting, Vacant, ReadyForUnlock, Vacant, Vacant]
    , testCase "present dismissal switch suppresses unlock" $ do
        let readyTime = addUTCTime (3 * 60 + 1) baseTime
            approachTime = addUTCTime (3 * 60 + 2) baseTime
            snapshots =
              [ TestUnlockSnapshot baseTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot readyTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot approachTime dismissedMqttSnapshot (exampleOccupiedSnapshot approachTime 4.5)
              ]
        runUnlockInputsWithMqtt dismissUnlockConfig snapshots @?= [Nothing, Nothing, Nothing]
    , testCase "absent dismissal switch allows unlock" $ do
        let readyTime = addUTCTime (3 * 60 + 1) baseTime
            approachTime = addUTCTime (3 * 60 + 2) baseTime
            snapshots =
              [ TestUnlockSnapshot baseTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot readyTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot approachTime emptyMqttSnapshot (exampleOccupiedSnapshot approachTime 4.5)
              ]
        runUnlockInputsWithMqtt dismissUnlockConfig snapshots @?= [Nothing, Nothing, Just Unlock]
    , testCase "approach while room remains occupied does not emit unlock after delay" $ do
        let waitingTime = addUTCTime (3 * 60 + 1) baseTime
            approachTime = addUTCTime (3 * 60 + 2) baseTime
            snapshots =
              [ TestSnapshot baseTime (exampleOccupiedSnapshot baseTime 6.0)
              , TestSnapshot waitingTime (exampleOccupiedSnapshot waitingTime 6.0)
              , TestSnapshot approachTime (exampleOccupiedSnapshot approachTime 4.5)
              ]
        runUnlockInputs exampleUnlockConfig snapshots @?= [Nothing, Nothing, Nothing]
    ]

data TestInput = TestInput
  { at :: !UTCTime
  , input :: !(Heartbeated ESPStatus)
  }

data TestSnapshot = TestSnapshot
  { at :: !UTCTime
  , snapshot :: !ESPresenseSnapshot
  }

data TestUnlockSnapshot = TestUnlockSnapshot
  { unlockAt :: !UTCTime
  , unlockMqtt :: !MqttSnapshot
  , unlockSnapshot :: !ESPresenseSnapshot
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

type UnlockS =
  ClSF
    (Eff '[Reader UnlockConfig])
    TestClock
    (MqttSnapshot, ESPresenseSnapshot)
    (Maybe UnlockEvent)

type UnlockFeedbackS =
  ClSF
    (Eff '[Reader UnlockConfig])
    TestClock
    (MqttSnapshot, ESPresenseSnapshot)
    (UnlockFeedback, Maybe UnlockEvent)

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

runUnlockInputs :: UnlockConfig -> [TestSnapshot] -> [Maybe UnlockEvent]
runUnlockInputs cfg inputs =
  runUnlockInputsWithMqtt
    cfg
    [ TestUnlockSnapshot at emptyMqttSnapshot snapshot
    | TestSnapshot {..} <- inputs
    ]

runUnlockInputsWithMqtt :: UnlockConfig -> [TestUnlockSnapshot] -> [Maybe UnlockEvent]
runUnlockInputsWithMqtt cfg inputs =
  runPureEff $ runReader cfg $ go unlockEventS Nothing inputs
  where
    go :: UnlockS -> Maybe UTCTime -> [TestUnlockSnapshot] -> Eff '[Reader UnlockConfig] [Maybe UnlockEvent]
    go _ _ [] = pure []
    go signal previous (TestUnlockSnapshot {..} : rest) = do
      let timeInfo =
            TimeInfo
              { sinceLast = maybe 0 (realToFrac . (unlockAt `diffUTCTime`)) previous
              , sinceInit = realToFrac $ unlockAt `diffUTCTime` baseTime
              , absolute = unlockAt
              , tag = ()
              }
      Result signal' event <- runReaderT (stepAutomaton signal (unlockMqtt, unlockSnapshot)) timeInfo
      (event :) <$> go signal' (Just unlockAt) rest

runUnlockFeedbackInputs :: UnlockConfig -> [TestSnapshot] -> [(UnlockFeedback, Maybe UnlockEvent)]
runUnlockFeedbackInputs cfg inputs =
  runPureEff $ runReader cfg $ go unlockFeedbackS Nothing inputs
  where
    go :: UnlockFeedbackS -> Maybe UTCTime -> [TestSnapshot] -> Eff '[Reader UnlockConfig] [(UnlockFeedback, Maybe UnlockEvent)]
    go _ _ [] = pure []
    go signal previous (TestSnapshot {..} : rest) = do
      let timeInfo =
            TimeInfo
              { sinceLast = maybe 0 (realToFrac . (at `diffUTCTime`)) previous
              , sinceInit = realToFrac $ at `diffUTCTime` baseTime
              , absolute = at
              , tag = ()
              }
      Result signal' event <- runReaderT (stepAutomaton signal (emptyMqttSnapshot, snapshot)) timeInfo
      (event :) <$> go signal' (Just at) rest

emptyMqttSnapshot :: MqttSnapshot
emptyMqttSnapshot = MqttSnapshot {switches = HM.empty}

dismissedMqttSnapshot :: MqttSnapshot
dismissedMqttSnapshot = MqttSnapshot {switches = HM.fromList [("do-not-disturb", True)]}

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

vacantSnapshot :: ESPresenseSnapshot
vacantSnapshot =
  ESPresenseSnapshot
    { sensors = HM.empty
    , rooms = HM.fromList [("home", [])]
    }

occupiedSnapshot :: UTCTime -> ESPresenseSnapshot
occupiedSnapshot timestamp =
  ESPresenseSnapshot
    { sensors = HM.fromList [("entrance", HM.fromList [("watch:", sensorStateAt timestamp 1)])]
    , rooms = HM.fromList [("home", [deviceStatus [("entrance", timestamp)]])]
    }

partialRoomOccupiedSnapshot :: UTCTime -> ESPresenseSnapshot
partialRoomOccupiedSnapshot timestamp =
  ESPresenseSnapshot
    { sensors =
        HM.fromList
          [ ("entrance", HM.fromList [("watch:", sensorStateAt timestamp 3.0)])
          , ("bedroom", HM.fromList [("watch:", sensorStateAt timestamp 1.0)])
          ]
    , rooms = HM.fromList [("home", [deviceStatus [("bedroom", timestamp)]])]
    }

exampleOccupiedSnapshot :: UTCTime -> Float -> ESPresenseSnapshot
exampleOccupiedSnapshot timestamp entranceDistance =
  ESPresenseSnapshot
    { sensors = HM.fromList [("entrance", HM.fromList [("watch:", sensorStateAt timestamp entranceDistance)])]
    , rooms = HM.fromList [("home", [deviceStatus [("entrance", timestamp)]])]
    }

exampleSensorOnlySnapshot :: UTCTime -> Float -> ESPresenseSnapshot
exampleSensorOnlySnapshot timestamp entranceDistance =
  ESPresenseSnapshot
    { sensors = HM.fromList [("entrance", HM.fromList [("watch:", sensorStateAt timestamp entranceDistance)])]
    , rooms = HM.fromList [("home", [])]
    }

exampleBedroomSnapshot :: UTCTime -> Float -> ESPresenseSnapshot
exampleBedroomSnapshot timestamp bedroomDistance =
  ESPresenseSnapshot
    { sensors = HM.fromList [("bedroom", HM.fromList [("watch:", sensorStateAt timestamp bedroomDistance)])]
    , rooms = HM.fromList [("home", [deviceStatus [("bedroom", timestamp)]])]
    }

exampleTwoSensorSnapshot :: UTCTime -> UTCTime -> ESPresenseSnapshot
exampleTwoSensorSnapshot bedroomTime entranceTime =
  ESPresenseSnapshot
    { sensors =
        HM.fromList
          [ ("bedroom", HM.fromList [("watch:", sensorStateAt bedroomTime 4.0)])
          , ("entrance", HM.fromList [("watch:", sensorStateAt entranceTime 4.5)])
          ]
    , rooms = HM.fromList [("home", [deviceStatus [("bedroom", bedroomTime), ("entrance", entranceTime)]])]
    }

examplePartialRoomOccupiedSnapshot :: UTCTime -> ESPresenseSnapshot
examplePartialRoomOccupiedSnapshot timestamp =
  ESPresenseSnapshot
    { sensors =
        HM.fromList
          [ ("entrance", HM.fromList [("watch:", sensorStateAt timestamp 7.0)])
          , ("bedroom", HM.fromList [("watch:", sensorStateAt timestamp 4.0)])
          ]
    , rooms = HM.fromList [("home", [deviceStatus [("bedroom", timestamp)]])]
    }

unlockConfig :: UnlockConfig
unlockConfig =
  UnlockConfig
    { room = "home"
    , delay = seconds 3
    , locks = []
    , approach =
        [ ApproachCondition
            { sensor = "entrance"
            , device = "watch:"
            , distance = 2
            }
        ]
    , dismiss = []
    }

exampleUnlockConfig :: UnlockConfig
exampleUnlockConfig =
  UnlockConfig
    { room = "home"
    , delay = minutes 3
    , locks = []
    , approach =
        [ ApproachCondition
            { sensor = "entrance"
            , device = "watch:"
            , distance = 5.0
            }
        ]
    , dismiss = []
    }

dismissUnlockConfig :: UnlockConfig
dismissUnlockConfig =
  exampleUnlockConfig
    { dismiss =
        [ DismissCondition
            { switch = "do-not-disturb"
            }
        ]
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

windowedPresenceConfig :: ESPresenseConfig
windowedPresenceConfig =
  ESPresenseConfig
    { devices = ["watch:"]
    , sensors =
        [ ESPSensor
            { name = "entrance"
            , max_distance = 8
            , skip_distance = 0.5
            , skip_ms = 2500
            , timeout = seconds 30
            , window = Just 3
            }
        , ESPSensor
            { name = "bedroom"
            , max_distance = 8
            , skip_distance = 0.5
            , skip_ms = 2500
            , timeout = seconds 30
            , window = Just 3
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

statusAt :: UTCTime -> ESPSensorName -> Float -> ESPStatus
statusAt timestamp sensor distance =
  ESPStatus
    { timestamp
    , sensor
    , mac = "5da2c2ab0a40"
    , id = "watch:"
    , name = "Watch"
    , rssi = -65
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
