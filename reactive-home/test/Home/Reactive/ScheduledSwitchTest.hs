{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Home.Reactive.ScheduledSwitchTest (test_scheduledSwitches, test_scheduledSwitchPublishing, test_scheduledSwitchSnapshot) where

import Control.Exception (evaluate, try)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, addUTCTime)
import Effectful (Eff, runPureEff, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Network.Mqtt (Mqtt (..), PublishResult (..), QoS (..))
import Effectful.Network.Mqtt qualified as Mqtt
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (State, execState, modify)
import FRP.Rhine (ClSF, Clock (..), Result (..), TimeInfo (..), stepAutomaton)
import Home.Reactive.Duration (seconds)
import Home.Reactive.MQTT (MqttDevices (..), MqttScheduledSwitch (..), MqttSnapshot (..), Topic, mqttSnapshotS)
import Home.Reactive.ScheduledSwitch (ScheduledSwitchEvent (..), ScheduledSwitchPublishError (..), publishScheduledSwitchEvents, scheduledSwitchEventsS, scheduledSwitchEventsWithS)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data TestClock = TestClock

instance Clock (Eff es) TestClock where
  type Time TestClock = UTCTime
  type Tag TestClock = ()

  initClock TestClock =
    error "TestClock is stepped manually in scheduled switch tests"

test_scheduledSwitches :: TestTree
test_scheduledSwitches =
  testGroup
    "scheduled switches"
    [ testCase "no configured switches produce no events" $
        runScheduled [] [0, 10, 12, 20] @?= replicate 4 []
    , testCase "startup advertises OFF once and waits for the first interval" $
        runScheduled [bumpSwitch] [0.5, 1, 9.99]
          @?= [[off bumpSwitch], [], []]
    , testCase "exact interval and on-duration boundaries emit one transition" $
        runScheduled [bumpSwitch] [0, 9.99, 10, 10, 11.99, 12, 12, 15]
          @?= [[off bumpSwitch], [], [on bumpSwitch], [], [], [off bumpSwitch], [], []]
    , testCase "subsequent ON events follow the interval cadence" $
        runScheduled [bumpSwitch] [0, 10, 12, 19.99, 20, 22, 30, 32]
          @?= [[off bumpSwitch], [on bumpSwitch], [off bumpSwitch], [], [on bumpSwitch], [off bumpSwitch], [on bumpSwitch], [off bumpSwitch]]
    , testCase "switches with different schedules run independently" $
        runScheduled [bumpSwitch, otherSwitch] [0, 6, 7, 10, 12, 13]
          @?= [ [off bumpSwitch, off otherSwitch]
              , [on otherSwitch]
              , [off otherSwitch]
              , [on bumpSwitch]
              , [off bumpSwitch, on otherSwitch]
              , [off otherSwitch]
              ]
    , testCase "a late ON tick still gets its full on_duration" $
        runScheduled [bumpSwitch] [0, 10.5, 12, 12.5, 20, 22]
          @?= [[off bumpSwitch], [on bumpSwitch], [], [off bumpSwitch], [on bumpSwitch], [off bumpSwitch]]
    , testCase "missed intervals produce one ON and resume the future cadence" $
        runScheduled [bumpSwitch] [0, 45, 46, 47, 49.99, 50, 52]
          @?= [[off bumpSwitch], [on bumpSwitch], [], [off bumpSwitch], [], [on bumpSwitch], [off bumpSwitch]]
    , testCase "an ON extending past the next interval skips that activation" $
        runScheduled [bumpSwitch] [0, 19, 20, 21, 21, 29.99, 30, 32]
          @?= [[off bumpSwitch], [on bumpSwitch], [], [off bumpSwitch], [], [], [on bumpSwitch], [off bumpSwitch]]
    , testCase "late OFF does not immediately replay a missed ON" $
        runScheduled [bumpSwitch] [0, 10, 35, 35, 39.99, 40, 42]
          @?= [[off bumpSwitch], [on bumpSwitch], [off bumpSwitch], [], [], [on bumpSwitch], [off bumpSwitch]]
    , testCase "blocked ON publication starts on_duration from completion" $ do
        let complete now events =
              pure $
                if now == addUTCTime 10 baseTime && any (.state) events
                  then addUTCTime 19 baseTime
                  else now
        runScheduledWith complete [bumpSwitch] [0, 10, 20, 21, 29.99, 30, 32]
          @?= [[off bumpSwitch], [on bumpSwitch], [], [off bumpSwitch], [], [on bumpSwitch], [off bumpSwitch]]
    , testCase "blocked initial OFF skips activations missed while publishing" $ do
        let complete now events =
              pure $
                if now == baseTime && events == [off bumpSwitch]
                  then addUTCTime 15 baseTime
                  else now
        runScheduledWith complete [bumpSwitch] [0, 15, 19.99, 20, 22]
          @?= [[off bumpSwitch], [], [], [on bumpSwitch], [off bumpSwitch]]
    , testCase "an off gap shorter than the heartbeat skips the overlapping cadence" $ do
        let fastSwitch = bumpSwitch {interval = seconds 1, on_duration = seconds 0.9}
        runScheduled [fastSwitch] [0, 0.5, 1, 1.5, 2, 2.5, 3]
          @?= [[off fastSwitch], [], [on fastSwitch], [], [off fastSwitch], [], [on fastSwitch]]
    ]

test_scheduledSwitchPublishing :: TestTree
test_scheduledSwitchPublishing =
  testGroup
    "scheduled switch publishing"
    [ testCase "each transition publishes retained boolean payloads with QoS 1" $
        capturePublications (publishScheduledSwitchEvents [off bumpSwitch, on bumpSwitch, off otherSwitch])
          @?= [ (bumpSwitch.topic, "false", QoS1, True)
              , (bumpSwitch.topic, "true", QoS1, True)
              , (otherSwitch.topic, "false", QoS1, True)
              ]
    , testCase "ticks without transitions publish nothing" $
        capturePublications (publishScheduledSwitchEvents []) @?= []
    , testCase "broker rejection reports the failed transition" $ do
        let event = on bumpSwitch
        result <-
          try @ScheduledSwitchPublishError $
            evaluate $
              capturePublicationsWith (AckedQoS1 Mqtt.NotAuthorized []) (publishScheduledSwitchEvents [event])
        result @?= Left (ScheduledSwitchPublishError event Mqtt.NotAuthorized)
    , testCase "a retained publication succeeds without current subscribers" $
        capturePublicationsWith (AckedQoS1 Mqtt.NoMatchingSubscribers []) (publishScheduledSwitchEvents [on bumpSwitch])
          @?= [(bumpSwitch.topic, "true", QoS1, True)]
    ]

test_scheduledSwitchSnapshot :: TestTree
test_scheduledSwitchSnapshot =
  testCase "scheduled switch MQTT states are available in the switch snapshot" $ do
    let devices = MqttDevices {switches = [], scheduled_switches = [bumpSwitch]}
        inputs =
          [ (0, message bumpSwitch.topic "true")
          , (1, message "unrelated/state" "false")
          , (2, message bumpSwitch.topic "invalid")
          , (3, message bumpSwitch.topic "false")
          ]
        snapshots = runPureEff $ runReader devices $ runSignal mqttSnapshotS inputs
    map (HM.lookup bumpSwitch.name . (.switches)) snapshots
      @?= [Just True, Just True, Just True, Just False]

runScheduled :: [MqttScheduledSwitch] -> [Double] -> [[ScheduledSwitchEvent]]
runScheduled switches times =
  runPureEff $ runSignal (scheduledSwitchEventsS switches) [(at, ()) | at <- times]

runScheduledWith :: (UTCTime -> [ScheduledSwitchEvent] -> Eff '[] UTCTime) -> [MqttScheduledSwitch] -> [Double] -> [[ScheduledSwitchEvent]]
runScheduledWith complete switches times =
  runPureEff $ runSignal (scheduledSwitchEventsWithS complete switches) [(at, ()) | at <- times]

runSignal :: ClSF (Eff es) TestClock a b -> [(Double, a)] -> Eff es [b]
runSignal initial = go initial Nothing
  where
    go _ _ [] = pure []
    go signal previous ((at, input) : rest) = do
      let timeInfo =
            TimeInfo
              { sinceLast = maybe 0 (at -) previous
              , sinceInit = at
              , absolute = addUTCTime (realToFrac at) baseTime
              , tag = ()
              }
      Result signal' output <- runReaderT (stepAutomaton signal input) timeInfo
      (output :) <$> go signal' (Just at) rest

type Publication = (Topic, ByteString, QoS, Bool)

capturePublications :: Eff '[Mqtt, State [Publication]] () -> [Publication]
capturePublications = capturePublicationsWith (AckedQoS1 Mqtt.Success [])

capturePublicationsWith :: PublishResult -> Eff '[Mqtt, State [Publication]] () -> [Publication]
capturePublicationsWith result = runPureEff . execState [] . recordPublications result

recordPublications :: (State [Publication] :> es) => PublishResult -> Eff (Mqtt : es) a -> Eff es a
recordPublications result = interpret_ \case
  Publish topic payload options -> do
    modify (<> [(topic, payload, options.qos, options.retain)])
    pure result
  _ -> error "Scheduled switches must only use MQTT Publish"

message :: Topic -> ByteString -> Mqtt.Message
message topic payload =
  Mqtt.Message
    { topic
    , payload
    , qos = QoS1
    , retain = True
    , dup = False
    , properties = []
    }

on :: MqttScheduledSwitch -> ScheduledSwitchEvent
on switch = ScheduledSwitchEvent {switch, state = True}

off :: MqttScheduledSwitch -> ScheduledSwitchEvent
off switch = ScheduledSwitchEvent {switch, state = False}

bumpSwitch :: MqttScheduledSwitch
bumpSwitch =
  MqttScheduledSwitch
    { name = "Homepod Bump"
    , topic = "delay/homepod-bump/state"
    , interval = seconds 10
    , on_duration = seconds 2
    }

otherSwitch :: MqttScheduledSwitch
otherSwitch =
  MqttScheduledSwitch
    { name = "Other switch"
    , topic = "delay/other/state"
    , interval = seconds 6
    , on_duration = seconds 1
    }

baseTime :: UTCTime
baseTime = read "2026-01-01 00:00:00 UTC"
