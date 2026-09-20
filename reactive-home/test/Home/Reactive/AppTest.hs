{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Home.Reactive.AppTest (test_configParsing, test_mqttConfigJson) where

import Data.Aeson qualified as A
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Home.Reactive.App (Config (..))
import Home.Reactive.ESPresense (seconds)
import Home.Reactive.MQTT (MqttDevices (..), MqttScheduledSwitch (..), mqttTopicFilters)
import Home.Reactive.Sesame5 (AutoLockDismissCondition (..), SesameConfig (..), SesameDevice (..), SesameUUID (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Toml (decodeExact, genericCodec)

test_configParsing :: TestTree
test_configParsing =
  testGroup
    "app config parsing"
    [ testCase "clientId is optional and defaults to broker-assigned mode" $
        decodeExact (genericCodec @Config) withoutClientIdToml
          @?= Right
            Config
              { host = Just "localhost"
              , port = Just 1883
              , clientId = Nothing
              , user = Nothing
              , password = Nothing
              , espresense = Nothing
              , sesame = Nothing
              , mackerel = Nothing
              , unlock = Nothing
              , logLevel = Nothing
              , mqtt = Nothing
              , switchbot = Nothing
              , hometrics = Nothing
              }
    , testCase "clientId preserves explicit stable client identifiers" $
        (.clientId) <$> decodeExact (genericCodec @Config) withClientIdToml
          @?= Right (Just "reactive-home-test")
    , testCase "MQTT switch table arrays parse as devices" $
        foldMap mqttTopicFilters . (.mqtt) <$> decodeExact (genericCodec @Config) mqttSwitchToml
          @?= Right ["switch/do-not-disturb/state"]
    , testCase "existing MQTT switches default to no scheduled switches" $
        (fmap (.scheduled_switches) . (.mqtt) <$> decodeExact (genericCodec @Config) mqttSwitchToml)
          @?= Right (Just [])
    , testCase "the scheduled-switch example parses without ordinary switches" $
        ((.mqtt) <$> decodeExact (genericCodec @Config) mqttScheduledSwitchToml)
          @?= Right
            ( Just
                MqttDevices
                  { switches = []
                  , scheduled_switches =
                      [ MqttScheduledSwitch
                          { name = "Homepod Bump"
                          , topic = "delay/homepod-bump/state"
                          , interval = seconds 300
                          , on_duration = seconds 10
                          }
                      ]
                  }
            )
    , testCase "ordinary and scheduled MQTT switches subscribe together" $
        (foldMap mqttTopicFilters . (.mqtt) <$> decodeExact (genericCodec @Config) mqttCombinedSwitchToml)
          @?= Right ["switch/do-not-disturb/state", "delay/homepod-bump/state"]
    , testCase "empty MQTT config defaults both switch lists" $
        ((.mqtt) <$> decodeExact (genericCodec @Config) (withoutClientIdToml <> "\n[mqtt]\n"))
          @?= Right (Just MqttDevices {switches = [], scheduled_switches = []})
    , testGroup
        "invalid scheduled switches"
        [ testCase label $
            case decodeExact (genericCodec @Config) input of
              Left _ -> pure ()
              Right cfg -> assertFailure $ "expected TOML decode failure, got: " <> show cfg
        | (label, input) <-
            [ ("malformed interval", T.replace "interval = \"5m\"" "interval = \"later\"" mqttScheduledSwitchToml)
            , ("malformed on_duration", T.replace "on_duration = \"10s\"" "on_duration = \"briefly\"" mqttScheduledSwitchToml)
            , ("zero interval", T.replace "interval = \"5m\"" "interval = \"0s\"" mqttScheduledSwitchToml)
            , ("zero on_duration", T.replace "on_duration = \"10s\"" "on_duration = \"0s\"" mqttScheduledSwitchToml)
            , ("negative interval", T.replace "interval = \"5m\"" "interval = \"-5m\"" mqttScheduledSwitchToml)
            , ("negative on_duration", T.replace "on_duration = \"10s\"" "on_duration = \"-10s\"" mqttScheduledSwitchToml)
            , ("non-finite interval", T.replace "5m" (T.replicate 400 "9" <> "s") mqttScheduledSwitchToml)
            , ("non-finite on_duration", T.replace "10s" (T.replicate 400 "9" <> "s") mqttScheduledSwitchToml)
            , ("on_duration equal to interval", T.replace "on_duration = \"10s\"" "on_duration = \"5m\"" mqttScheduledSwitchToml)
            , ("on_duration greater than interval", T.replace "on_duration = \"10s\"" "on_duration = \"6m\"" mqttScheduledSwitchToml)
            , ("missing interval", T.replace "interval = \"5m\"" "" mqttScheduledSwitchToml)
            , ("missing on_duration", T.replace "on_duration = \"10s\"" "" mqttScheduledSwitchToml)
            , ("invalid scheduled-switch collection", withoutClientIdToml <> "\n[mqtt]\nscheduled_switches = \"invalid\"\n")
            ]
        ]
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

test_mqttConfigJson :: TestTree
test_mqttConfigJson =
  testGroup
    "MQTT JSON configuration"
    [ testCase "missing switch collections default to empty" $
        A.eitherDecode @MqttDevices "{}"
          @?= Right MqttDevices {switches = [], scheduled_switches = []}
    , testCase "scheduled-switch encoding round-trips" $
        A.eitherDecode (A.encode scheduledMqttDevices) @?= Right scheduledMqttDevices
    , testCase "scheduled-switch JSON values round-trip" $
        A.fromJSON (A.toJSON scheduledMqttDevices) @?= A.Success scheduledMqttDevices
    , testGroup
        "rejects invalid durations"
        [ testCase label $
            case A.eitherDecode @MqttDevices (A.encode invalid) of
              Left _ -> pure ()
              Right devices -> assertFailure $ "expected JSON decode failure, got: " <> show devices
        | (label, interval, onDuration) <-
            [ ("zero interval", "0s", "10s")
            , ("zero on_duration", "5m", "0s")
            , ("on_duration equal to interval", "5m", "5m")
            , ("on_duration greater than interval", "5m", "6m")
            ]
        , let invalid =
                A.object
                  [ "scheduled_switches"
                      A..= [ A.object
                               [ "name" A..= ("Homepod Bump" :: T.Text)
                               , "topic" A..= ("delay/homepod-bump/state" :: T.Text)
                               , "interval" A..= (interval :: T.Text)
                               , "on_duration" A..= (onDuration :: T.Text)
                               ]
                           ]
                  ]
        ]
    ]

scheduledMqttDevices :: MqttDevices
scheduledMqttDevices =
  MqttDevices
    { switches = []
    , scheduled_switches =
        [ MqttScheduledSwitch
            { name = "Homepod Bump"
            , topic = "delay/homepod-bump/state"
            , interval = seconds 300
            , on_duration = seconds 10
            }
        ]
    }

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

mqttScheduledSwitchToml :: T.Text
mqttScheduledSwitchToml =
  """
  host = "localhost"
  port = 1883

  [mqtt]
  [[mqtt.scheduled_switches]]
  name = "Homepod Bump"
  topic = "delay/homepod-bump/state"
  interval = "5m"
  on_duration = "10s"
  """

mqttCombinedSwitchToml :: T.Text
mqttCombinedSwitchToml =
  mqttSwitchToml
    <> "\n"
    <> """
       [[mqtt.scheduled_switches]]
       name = "Homepod Bump"
       topic = "delay/homepod-bump/state"
       interval = "5m"
       on_duration = "10s"
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
