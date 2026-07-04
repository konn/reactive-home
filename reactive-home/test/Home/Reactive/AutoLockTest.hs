{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Home.Reactive.AutoLockTest (test_autolock) where

import Control.Monad.Trans.Reader (runReaderT)
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static (Reader, runReader)
import FRP.Rhine (ClSF, Clock (..), Result (..), TimeInfo (..), stepAutomaton)
import Home.Reactive.AutoLock (AutoLockEvent (..), autolockEventS)
import Home.Reactive.ESPresense (Heartbeated (..), seconds)
import Home.Reactive.MQTT (MqttSnapshot (..))
import Home.Reactive.Sesame5
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data TestClock = TestClock

instance Clock (Eff es) TestClock where
  type Time TestClock = UTCTime
  type Tag TestClock = ()

  initClock TestClock =
    error "TestClock is stepped manually in AutoLock tests"

test_autolock :: TestTree
test_autolock =
  testGroup
    "autolock"
    [ testCase "unlocked status starts timer and emits lock after timeout" $ do
        let unlockTime = addUTCTime 1 baseTime
            beforeTimeout = addUTCTime 3.9 baseTime
            afterTimeout = addUTCTime 4.1 baseTime
            outputs =
              runAutoLockInputs
                autoLockEnv
                [ TestInput unlockTime (Event $ sesameStatus unlockTime UNLOCKED)
                , TestInput beforeTimeout Heartbeat
                , TestInput afterTimeout Heartbeat
                ]
        outputs @?= [[], [], [AutoLockEvent "front" autoLockDevice]]
    , testCase "device without autolock_timeout never starts timer" $ do
        let unlockTime = addUTCTime 1 baseTime
            afterTimeout = addUTCTime 60 baseTime
            outputs =
              runAutoLockInputs
                noAutoLockEnv
                [ TestInput unlockTime (Event $ sesameStatus unlockTime UNLOCKED)
                , TestInput afterTimeout Heartbeat
                ]
        outputs @?= [[], []]
    , testCase "lock status during timeout cancels timer" $ do
        let unlockTime = addUTCTime 1 baseTime
            lockTime = addUTCTime 2 baseTime
            afterTimeout = addUTCTime 4.1 baseTime
            outputs =
              runAutoLockInputs
                autoLockEnv
                [ TestInput unlockTime (Event $ sesameStatus unlockTime UNLOCKED)
                , TestInput lockTime (Event $ sesameStatus lockTime LOCKED)
                , TestInput afterTimeout Heartbeat
                ]
        outputs @?= [[], [], []]
    , testCase "second unlock during timeout does not alter original timer" $ do
        let unlockTime = addUTCTime 1 baseTime
            secondUnlockTime = addUTCTime 2 baseTime
            afterOriginalTimeout = addUTCTime 4.1 baseTime
            afterRestartedTimeout = addUTCTime 5.1 baseTime
            outputs =
              runAutoLockInputs
                autoLockEnv
                [ TestInput unlockTime (Event $ sesameStatus unlockTime UNLOCKED)
                , TestInput secondUnlockTime (Event $ sesameStatus secondUnlockTime UNLOCKED)
                , TestInput afterOriginalTimeout Heartbeat
                , TestInput afterRestartedTimeout Heartbeat
                ]
        outputs @?= [[], [], [AutoLockEvent "front" autoLockDevice], []]
    , testCase "dismissal switch prevents starting timer" $ do
        let unlockTime = addUTCTime 1 baseTime
            afterTimeout = addUTCTime 4.1 baseTime
            outputs =
              runAutoLockInputsWithMqtt
                dismissedMqttSnapshot
                dismissedAutoLockEnv
                [ TestInput unlockTime (Event $ sesameStatus unlockTime UNLOCKED)
                , TestInput afterTimeout Heartbeat
                ]
        outputs @?= [[], []]
    , testCase "dismissal switch prevents firing pending timer" $ do
        let unlockTime = addUTCTime 1 baseTime
            afterTimeout = addUTCTime 4.1 baseTime
            outputs =
              runAutoLockMqttInputs
                dismissedAutoLockEnv
                [ TestMqttInput unlockTime emptyMqttSnapshot (Event $ sesameStatus unlockTime UNLOCKED)
                , TestMqttInput afterTimeout dismissedMqttSnapshot Heartbeat
                ]
        outputs @?= [[], []]
    ]

data TestInput = TestInput
  { at :: !UTCTime
  , input :: !(Heartbeated SesameStatus)
  }

data TestMqttInput = TestMqttInput
  { mqttAt :: !UTCTime
  , mqttSnapshot :: !MqttSnapshot
  , mqttInput :: !(Heartbeated SesameStatus)
  }

type AutoLockS =
  ClSF
    (Eff '[Reader SesameEnv])
    TestClock
    (MqttSnapshot, Heartbeated SesameStatus)
    [AutoLockEvent]

runAutoLockInputs :: SesameEnv -> [TestInput] -> [[AutoLockEvent]]
runAutoLockInputs env inputs =
  runAutoLockInputsWithMqtt emptyMqttSnapshot env inputs

runAutoLockInputsWithMqtt :: MqttSnapshot -> SesameEnv -> [TestInput] -> [[AutoLockEvent]]
runAutoLockInputsWithMqtt mqtt env inputs =
  runAutoLockMqttInputs
    env
    [ TestMqttInput at mqtt input
    | TestInput {..} <- inputs
    ]

runAutoLockMqttInputs :: SesameEnv -> [TestMqttInput] -> [[AutoLockEvent]]
runAutoLockMqttInputs env inputs =
  runPureEff $ runReader env $ go autolockEventS Nothing inputs
  where
    go :: AutoLockS -> Maybe UTCTime -> [TestMqttInput] -> Eff '[Reader SesameEnv] [[AutoLockEvent]]
    go _ _ [] = pure []
    go signal previous (TestMqttInput {..} : rest) = do
      let timeInfo =
            TimeInfo
              { sinceLast = maybe 0 (realToFrac . (mqttAt `diffUTCTime`)) previous
              , sinceInit = realToFrac $ mqttAt `diffUTCTime` baseTime
              , absolute = mqttAt
              , tag = ()
              }
      Result signal' events <- runReaderT (stepAutomaton signal (mqttSnapshot, mqttInput)) timeInfo
      (events :) <$> go signal' (Just mqttAt) rest

sesameStatus :: UTCTime -> LockStatus -> SesameStatus
sesameStatus timestamp lockCurrentState =
  SesameStatus
    { name = "front"
    , uuid = "01234567-89ab-cdef-0123-456789abcdef"
    , lastUpdated = timestamp
    , position = 0
    , lockCurrentState
    , batteryVoltage = 6.0
    , batteryLevel = 100
    , statusLowBattery = False
    }

autoLockEnv :: SesameEnv
autoLockEnv =
  fromSesameConfig
    SesameConfig
      { prefix = "haskesame"
      , devices = HM.fromList [("front", autoLockDevice)]
      }

noAutoLockEnv :: SesameEnv
noAutoLockEnv =
  fromSesameConfig
    SesameConfig
      { prefix = "haskesame"
      , devices = HM.fromList [("front", noAutoLockDevice)]
      }

dismissedAutoLockEnv :: SesameEnv
dismissedAutoLockEnv =
  fromSesameConfig
    SesameConfig
      { prefix = "haskesame"
      , devices = HM.fromList [("front", dismissedAutoLockDevice)]
      }

autoLockDevice :: SesameDevice
autoLockDevice =
  SesameDevice
    { uuid = "01234567-89ab-cdef-0123-456789abcdef"
    , autolock_timeout = Just $ seconds 3
    , autolock_dismiss = []
    }

noAutoLockDevice :: SesameDevice
noAutoLockDevice =
  autoLockDevice {autolock_timeout = Nothing}

dismissedAutoLockDevice :: SesameDevice
dismissedAutoLockDevice =
  autoLockDevice
    { autolock_dismiss =
        [ AutoLockDismissCondition
            { switch = "do-not-disturb"
            }
        ]
    }

emptyMqttSnapshot :: MqttSnapshot
emptyMqttSnapshot = MqttSnapshot {switches = HM.empty}

dismissedMqttSnapshot :: MqttSnapshot
dismissedMqttSnapshot = MqttSnapshot {switches = HM.fromList [("do-not-disturb", True)]}

baseTime :: UTCTime
baseTime = read "2026-01-01 00:00:00 UTC"
