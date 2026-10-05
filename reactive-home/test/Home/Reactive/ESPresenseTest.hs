{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Home.Reactive.ESPresenseTest (test_tomlParsing, test_roomAbsence, test_deltas, test_unlockHeartbeat, test_unlockDismissalReset) where

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
  Duration,
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
    , testCase "dismissal after vacancy still permits exactly one approach unlock" $ do
        let readyTime = addUTCTime (3 * 60 + 1) baseTime
            dismissalTime = addUTCTime 1 readyTime
            approachTime = addUTCTime 2 readyTime
            stillNearTime = addUTCTime 3 readyTime
            dismissalOffTime = addUTCTime 4 readyTime
            snapshots =
              [ TestUnlockSnapshot baseTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot readyTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot dismissalTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot approachTime dismissedMqttSnapshot (exampleOccupiedSnapshot approachTime 4.5)
              , TestUnlockSnapshot stillNearTime dismissedMqttSnapshot (exampleOccupiedSnapshot stillNearTime 4.5)
              , TestUnlockSnapshot dismissalOffTime dismissalOffMqttSnapshot (exampleOccupiedSnapshot dismissalOffTime 4.5)
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt dismissUnlockConfig snapshots
        snd <$> feedbacks @?= [Nothing, Nothing, Nothing, Just Unlock, Nothing, Nothing]
        (.status) . fst <$> feedbacks @?= [Waiting, Vacant, Vacant, Occupied, Occupied, Occupied]
    , testGroup
        "dismissal preserves qualified vacancy through far room presence and expiry"
        [ testCase label $ do
            let readyTime = addUTCTime (3 * 60 + 1) baseTime
                bedroomTime = addUTCTime 1 readyTime
                expiryTime = addUTCTime 2 readyTime
                returnTime = addUTCTime 3 readyTime
                approachTime = addUTCTime 4 readyTime
                snapshots =
                  [ TestUnlockSnapshot baseTime emptyMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot readyTime emptyMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot bedroomTime bedroomMqtt (exampleBedroomSnapshot bedroomTime 4.0)
                  , TestUnlockSnapshot expiryTime dismissedMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot returnTime dismissedMqttSnapshot (exampleBedroomSnapshot returnTime 4.0)
                  , TestUnlockSnapshot approachTime dismissedMqttSnapshot (exampleTwoSensorSnapshot returnTime approachTime)
                  ]
                feedbacks = runUnlockFeedbackInputsWithMqtt dismissUnlockConfig snapshots
            snd <$> feedbacks @?= [Nothing, Nothing, Nothing, Nothing, Nothing, Just Unlock]
            (.status) . fst <$> feedbacks @?= [Waiting, Vacant, ReadyForUnlock, Vacant, ReadyForUnlock, Occupied]
        | (label, bedroomMqtt) <-
            [ ("dismissal starts before room reoccupies", dismissedMqttSnapshot)
            , ("dismissal starts after room reoccupies", emptyMqttSnapshot)
            ]
        ]
    , testGroup
        "dismissal prevents qualifying a new vacancy"
        [ testCase label $ do
            let beforeDeadline = addUTCTime (3 * 60 - 1) baseTime
                deadline = addUTCTime (3 * 60) baseTime
                afterDeadline = addUTCTime (3 * 60 + 1) baseTime
                approachTime = addUTCTime (3 * 60 + 2) baseTime
                snapshots =
                  [ TestUnlockSnapshot baseTime initialMqtt vacantSnapshot
                  , TestUnlockSnapshot beforeDeadline dismissedMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot deadline dismissedMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot afterDeadline dismissedMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot approachTime dismissedMqttSnapshot (exampleOccupiedSnapshot approachTime 4.5)
                  ]
                feedbacks = runUnlockFeedbackInputsWithMqtt dismissUnlockConfig snapshots
            snd <$> feedbacks @?= replicate 5 Nothing
            (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Waiting, Waiting, Occupied]
        | (label, initialMqtt) <-
            [ ("dismissal is already on at startup", dismissedMqttSnapshot)
            , ("dismissal turns on while waiting", emptyMqttSnapshot)
            ]
        ]
    , testCase "turning dismissal off discards elapsed vacancy before an approach" $ do
        let readyTime = addUTCTime (3 * 60 + 1) baseTime
            dismissalOffTime = addUTCTime 1 readyTime
            approachTime = addUTCTime 2 readyTime
            snapshots =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot readyTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot dismissalOffTime dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot approachTime dismissalOffMqttSnapshot (exampleOccupiedSnapshot approachTime 4.5)
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt dismissUnlockConfig snapshots
        snd <$> feedbacks @?= replicate 4 Nothing
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Waiting, Occupied]
    , testCase "dismissal prevents rearming after a qualified approach unlock" $ do
        let readyTime = addUTCTime (3 * 60 + 1) baseTime
            approachTime = addUTCTime 1 readyTime
            vacantTime = addUTCTime 2 readyTime
            elapsedTime = addUTCTime (3 * 60 + 1) vacantTime
            secondApproachTime = addUTCTime 1 elapsedTime
            snapshots =
              [ TestUnlockSnapshot baseTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot readyTime emptyMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot approachTime dismissedMqttSnapshot (exampleOccupiedSnapshot approachTime 4.5)
              , TestUnlockSnapshot vacantTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot elapsedTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot secondApproachTime dismissedMqttSnapshot (exampleOccupiedSnapshot secondApproachTime 4.5)
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt dismissUnlockConfig snapshots
        snd <$> feedbacks @?= [Nothing, Nothing, Just Unlock, Nothing, Nothing, Nothing]
        (.status) . fst <$> feedbacks @?= [Waiting, Vacant, Occupied, Waiting, Waiting, Occupied]
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

test_unlockDismissalReset :: TestTree
test_unlockDismissalReset =
  testGroup
    "unlock dismissal reset"
    [ testCase "empty room starts a fresh three-minute check followed by thirty seconds" $ do
        let offTime = addUTCTime 600 baseTime
            sample elapsed snapshot = TestUnlockSnapshot (addUTCTime elapsed offTime) dismissalOffMqttSnapshot snapshot
            inputs =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , sample 0 vacantSnapshot
              , sample 179.5 vacantSnapshot
              , sample 180 vacantSnapshot
              , sample 209.5 vacantSnapshot
              , sample 210 vacantSnapshot
              , sample 211 (exampleOccupiedSnapshot (addUTCTime 211 offTime) 4.5)
              , sample 212 (exampleOccupiedSnapshot (addUTCTime 212 offTime) 4.5)
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
        snd <$> feedbacks @?= replicate 6 Nothing <> [Just Unlock, Nothing]
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Waiting, Waiting, Waiting, Vacant, Occupied, Occupied]
        (.duration) . fst <$> feedbacks @?= [0, 0, 0, 0, 29.5, 30, 0, 0]
        (.rechecking) . fst <$> feedbacks @?= [False, True, True, False, False, False, False, False]
        (.occupied) . fst <$> feedbacks @?= [False, False, False, False, False, False, True, True]
    , testCase "sparse heartbeats count from the room recheck deadline" $ do
        let inputs =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 100 baseTime) dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 311 baseTime) dismissalOffMqttSnapshot vacantSnapshot
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Vacant]
        (.duration) . fst <$> feedbacks @?= [0, 0, 31]
    , testCase "DND ending while room presence is ageing starts a full room recheck" $ do
        let occupied = exampleBedroomSnapshot baseTime 4.0
            sample elapsed mqtt snapshot = TestUnlockSnapshot (addUTCTime elapsed baseTime) mqtt snapshot
            inputs =
              [ sample 0 dismissedMqttSnapshot occupied
              , sample 100 dismissalOffMqttSnapshot occupied
              , sample 179.5 dismissalOffMqttSnapshot occupied
              , sample 180 dismissalOffMqttSnapshot vacantSnapshot
              , sample 280 dismissalOffMqttSnapshot vacantSnapshot
              , sample 310 dismissalOffMqttSnapshot vacantSnapshot
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
        (.status) . fst <$> feedbacks @?= [Occupied, Occupied, Occupied, Waiting, Waiting, Vacant]
        (.duration) . fst <$> feedbacks @?= [0, 0, 0, 0, 0, 30]
        (.occupied) . fst <$> feedbacks @?= [True, True, True, False, False, False]
    , testCase "morning reporting gaps cannot preserve a pre-DND vacancy into the later approach" $ do
        let offTime = addUTCTime 60.5 baseTime
            bedroomTime = addUTCTime 67 baseTime
            emptyTime = addUTCTime 89 baseTime
            returnTime = addUTCTime 125 baseTime
            approachTime = addUTCTime 442 baseTime
            inputs =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot offTime dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot bedroomTime dismissalOffMqttSnapshot (exampleBedroomSnapshot bedroomTime 4.0)
              , TestUnlockSnapshot emptyTime dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot returnTime dismissalOffMqttSnapshot (exampleBedroomSnapshot returnTime 4.0)
              , TestUnlockSnapshot approachTime dismissalOffMqttSnapshot (exampleTwoSensorSnapshot returnTime approachTime)
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
        snd <$> feedbacks @?= replicate 6 Nothing
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Occupied, Waiting, Occupied, Occupied]
    , testCase "presence after DND turns off extends the wait beyond the fresh room check" $ do
        let offTime = addUTCTime 600 baseTime
            seenTime = addUTCTime 170 offTime
            expiryTime = addUTCTime 350 offTime
            inputs =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot offTime dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot seenTime dismissalOffMqttSnapshot (exampleBedroomSnapshot seenTime 4.0)
              , TestUnlockSnapshot (addUTCTime 180 offTime) dismissalOffMqttSnapshot (exampleBedroomSnapshot seenTime 4.0)
              , TestUnlockSnapshot (addUTCTime 349.5 offTime) dismissalOffMqttSnapshot (exampleBedroomSnapshot seenTime 4.0)
              , TestUnlockSnapshot expiryTime dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 29.5 expiryTime) dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 30 expiryTime) dismissalOffMqttSnapshot vacantSnapshot
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
        snd <$> feedbacks @?= replicate 8 Nothing
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Occupied, Occupied, Occupied, Waiting, Waiting, Vacant]
        (.duration) . fst <$> feedbacks @?= [0, 0, 0, 0, 0, 0, 29.5, 30]
    , testGroup
        "turning dismissal off revokes qualification before a simultaneous approach"
        [ testCase label $ do
            let onTime = addUTCTime 31 baseTime
                offTime = addUTCTime 32 baseTime
                inputs =
                  [ TestUnlockSnapshot baseTime emptyMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot (addUTCTime 30 baseTime) emptyMqttSnapshot vacantSnapshot
                  , TestUnlockSnapshot onTime dismissedMqttSnapshot (qualifiedSnapshot onTime)
                  , TestUnlockSnapshot offTime dismissalOffMqttSnapshot (exampleOccupiedSnapshot offTime 4.5)
                  ]
                feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
            snd <$> feedbacks @?= replicate 4 Nothing
            (.status) . fst <$> feedbacks @?= [Waiting, Vacant, qualifiedStatus, Occupied]
            (.duration) (fst $ last feedbacks) @?= 0
            (.rechecking) (fst $ last feedbacks) @?= True
        | (label, qualifiedSnapshot, qualifiedStatus) <-
            [ ("vacant", const vacantSnapshot, Vacant)
            , ("ready for unlock", (\at -> exampleBedroomSnapshot at 4.0), ReadyForUnlock)
            ]
        ]
    , testCase "each new ON-to-OFF transition resets both timers" $ do
        let sample elapsed mqtt = TestUnlockSnapshot (addUTCTime elapsed baseTime) mqtt vacantSnapshot
            inputs =
              [ sample 0 dismissedMqttSnapshot
              , sample 10 dismissalOffMqttSnapshot
              , sample 190 dismissalOffMqttSnapshot
              , sample 210 dismissedMqttSnapshot
              , sample 211 dismissalOffMqttSnapshot
              , sample 390.5 dismissalOffMqttSnapshot
              , sample 391 dismissalOffMqttSnapshot
              , sample 420.5 dismissalOffMqttSnapshot
              , sample 421 dismissalOffMqttSnapshot
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
        snd <$> feedbacks @?= replicate 9 Nothing
        (.status) . fst <$> feedbacks @?= replicate 8 Waiting <> [Vacant]
        (.duration) . fst <$> feedbacks @?= [0, 0, 0, 0, 0, 0, 0, 29.5, 30]
        (.rechecking) . fst <$> feedbacks @?= [False, True, False, False, True, True, False, False, False]
    , testCase "only the final configured dismissal turning off starts the recheck" $ do
        let cfg = liveUnlockConfig {dismiss = [DismissCondition "do-not-disturb", DismissCondition "guest"]}
            mqtt dnd guest unrelated = MqttSnapshot {switches = HM.fromList [("do-not-disturb", dnd), ("guest", guest), ("unrelated", unrelated)]}
            sample elapsed switches = TestUnlockSnapshot (addUTCTime elapsed baseTime) switches vacantSnapshot
            inputs =
              [ sample 0 (mqtt True True True)
              , sample 100 (mqtt False True True)
              , sample 200 (mqtt False True False)
              , sample 300 (mqtt False False False)
              , sample 400 (mqtt False False True)
              , sample 480 (mqtt False False False)
              , sample 509.5 (mqtt False False False)
              , sample 510 (mqtt False False False)
              ]
            feedbacks = runUnlockFeedbackInputsWithMqtt cfg inputs
        snd <$> feedbacks @?= replicate 8 Nothing
        (.status) . fst <$> feedbacks @?= replicate 7 Waiting <> [Vacant]
        (.duration) . fst <$> feedbacks @?= [0, 0, 0, 0, 0, 0, 29.5, 30]
        (.rechecking) . fst <$> feedbacks @?= [False, False, False, True, True, False, False, False]
    , testGroup
        "startup without an active dismissal keeps the existing delay"
        [ testCase label $ do
            let inputs =
                  [ TestUnlockSnapshot baseTime mqtt vacantSnapshot
                  , TestUnlockSnapshot (addUTCTime 29.5 baseTime) mqtt vacantSnapshot
                  , TestUnlockSnapshot (addUTCTime 30 baseTime) mqtt vacantSnapshot
                  ]
                feedbacks = runUnlockFeedbackInputsWithMqtt liveUnlockConfig inputs
            (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Vacant]
            (.rechecking) . fst <$> feedbacks @?= replicate 3 False
        | (label, mqtt) <- [("explicitly off", dismissalOffMqttSnapshot), ("absent", emptyMqttSnapshot)]
        ]
    , testCase "uses both configured durations instead of fixed three-minute and thirty-second values" $ do
        let cfg = liveUnlockConfig {delay = seconds 7}
            sample elapsed = TestUnlockSnapshot (addUTCTime elapsed baseTime) dismissalOffMqttSnapshot vacantSnapshot
            inputs =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , sample 100
              , sample 104.5
              , sample 105
              , sample 111.5
              , sample 112
              ]
            feedbacks = runUnlockFeedbackInputsWithTimeout (seconds 5) cfg inputs
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Waiting, Waiting, Waiting, Vacant]
        (.duration) . fst <$> feedbacks @?= [0, 0, 0, 0, 6.5, 7]
        (.rechecking) . fst <$> feedbacks @?= [False, True, True, False, False, False]
    , testCase "uses the configured room timeout and still enforces it with zero unlock delay" $ do
        let cfg = liveUnlockConfig {delay = seconds 0}
            inputs =
              [ TestUnlockSnapshot baseTime dismissedMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 100 baseTime) dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 104.5 baseTime) dismissalOffMqttSnapshot vacantSnapshot
              , TestUnlockSnapshot (addUTCTime 105 baseTime) dismissalOffMqttSnapshot vacantSnapshot
              ]
            feedbacks = runUnlockFeedbackInputsWithTimeout (seconds 5) cfg inputs
        (.status) . fst <$> feedbacks @?= [Waiting, Waiting, Waiting, Vacant]
        (.rechecking) . fst <$> feedbacks @?= [False, True, True, False]
    ]
  where
    liveUnlockConfig = dismissUnlockConfig {delay = seconds 30}

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
  runPureEff $ runReader cfg $ go (unlockEventS $ minutes 3) Nothing inputs
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
  runUnlockFeedbackInputsWithMqtt
    cfg
    [ TestUnlockSnapshot at emptyMqttSnapshot snapshot
    | TestSnapshot {..} <- inputs
    ]

runUnlockFeedbackInputsWithMqtt :: UnlockConfig -> [TestUnlockSnapshot] -> [(UnlockFeedback, Maybe UnlockEvent)]
runUnlockFeedbackInputsWithMqtt = runUnlockFeedbackInputsWithTimeout $ minutes 3

runUnlockFeedbackInputsWithTimeout :: Duration -> UnlockConfig -> [TestUnlockSnapshot] -> [(UnlockFeedback, Maybe UnlockEvent)]
runUnlockFeedbackInputsWithTimeout roomTimeout cfg inputs =
  runPureEff $ runReader cfg $ go (unlockFeedbackS roomTimeout) Nothing inputs
  where
    go :: UnlockFeedbackS -> Maybe UTCTime -> [TestUnlockSnapshot] -> Eff '[Reader UnlockConfig] [(UnlockFeedback, Maybe UnlockEvent)]
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

emptyMqttSnapshot :: MqttSnapshot
emptyMqttSnapshot = MqttSnapshot {switches = HM.empty}

dismissedMqttSnapshot :: MqttSnapshot
dismissedMqttSnapshot = MqttSnapshot {switches = HM.fromList [("do-not-disturb", True)]}

dismissalOffMqttSnapshot :: MqttSnapshot
dismissalOffMqttSnapshot = MqttSnapshot {switches = HM.fromList [("do-not-disturb", False)]}

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
