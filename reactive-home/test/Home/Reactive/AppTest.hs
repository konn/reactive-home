{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Home.Reactive.AppTest (test_configParsing) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Home.Reactive.App (Config (..))
import Home.Reactive.ESPresense (seconds)
import Home.Reactive.MQTT (mqttTopicFilters)
import Home.Reactive.Sesame5 (AutoLockDismissCondition (..), SesameConfig (..), SesameDevice (..), SesameUUID (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Toml (decodeExact, genericCodec)

test_configParsing :: TestTree
test_configParsing =
  testGroup
    "app config parsing"
    [ testCase "clientId is optional and defaults to broker-assigned mode" $
        decodeExact (genericCodec @Config) withoutClientIdToml
          @?= Right
            Config
              { host = "localhost"
              , port = 1883
              , clientId = Nothing
              , user = Nothing
              , password = Nothing
              , espresense = Nothing
              , sesame = Nothing
              , mackerel = Nothing
              , unlock = Nothing
              , logLevel = Nothing
              , mqtt = Nothing
              }
    , testCase "clientId preserves explicit stable client identifiers" $
        (.clientId) <$> decodeExact (genericCodec @Config) withClientIdToml
          @?= Right (Just "reactive-home-test")
    , testCase "MQTT switch table arrays parse as devices" $
        foldMap mqttTopicFilters . (.mqtt) <$> decodeExact (genericCodec @Config) mqttSwitchToml
          @?= Right ["switch/do-not-disturb/state"]
    , testCase "dismissal switch config subscribes to matching MQTT switch" $
        (foldMap mqttTopicFilters . (.mqtt) <$> decodeExact (genericCodec @Config) dismissSwitchToml)
          @?= Right ["switch/do-not-disturb/state"]
    , testCase "dismissal switch config without MQTT switch has no switch subscription" $
        (foldMap mqttTopicFilters . (.mqtt) <$> decodeExact (genericCodec @Config) dismissWithoutMqttSwitchToml)
          @?= Right []
    , testCase "Sesame autolock timeout is optional per device" $
        (fmap (.devices) . (.sesame) <$> decodeExact (genericCodec @Config) sesameAutoLockToml)
          @?= Right
            ( Just $
                HM.fromList
                  [
                    ( "front"
                    , SesameDevice
                        { uuid = UUID "01234567-89ab-cdef-0123-456789abcdef"
                        , autolock_timeout = Just $ seconds 30
                        , autolock_dismiss =
                            [ AutoLockDismissCondition
                                { switch = "do-not-disturb"
                                }
                            ]
                        }
                    )
                  ,
                    ( "back"
                    , SesameDevice
                        { uuid = UUID "fedcba98-7654-3210-fedc-ba9876543210"
                        , autolock_timeout = Nothing
                        , autolock_dismiss = []
                        }
                    )
                  ]
            )
    ]

withoutClientIdToml :: T.Text
withoutClientIdToml =
  """
  host = "localhost"
  port = 1883
  """

withClientIdToml :: T.Text
withClientIdToml =
  """
  host = "localhost"
  port = 1883
  clientId = "reactive-home-test"
  """

mqttSwitchToml :: T.Text
mqttSwitchToml =
  """
  host = "localhost"
  port = 1883

  [mqtt]

  [[mqtt.switches]]
  name = "do-not-disturb"
  topic = "switch/do-not-disturb/state"
  """

dismissSwitchToml :: T.Text
dismissSwitchToml =
  """
  host = "localhost"
  port = 1883

  [unlock]
  room = "home"
  delay = "3m"
  locks = []

  [[unlock.approach]]
  sensor = "entrance"
  device = "watch:"
  distance = 5.0

  [[unlock.dismiss]]
  switch = "do-not-disturb"

  [mqtt]

  [[mqtt.switches]]
  name = "do-not-disturb"
  topic = "switch/do-not-disturb/state"
  """

dismissWithoutMqttSwitchToml :: T.Text
dismissWithoutMqttSwitchToml =
  """
  host = "localhost"
  port = 1883

  [unlock]
  room = "home"
  delay = "3m"
  locks = []

  [[unlock.approach]]
  sensor = "entrance"
  device = "watch:"
  distance = 5.0

  [[unlock.dismiss]]
  switch = "do-not-disturb"
  """

sesameAutoLockToml :: T.Text
sesameAutoLockToml =
  """
  host = "localhost"
  port = 1883

  [sesame]
  prefix = "haskesame"

  [[sesame.devices]]
  key = "front"
  [sesame.devices.val]
  uuid = "01234567-89ab-cdef-0123-456789abcdef"
  autolock_timeout = "30s"

  [[sesame.devices.val.autolock_dismiss]]
  switch = "do-not-disturb"

  [[sesame.devices]]
  key = "back"
  [sesame.devices.val]
  uuid = "fedcba98-7654-3210-fedc-ba9876543210"
  """
