{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Home.Reactive.AppTest (test_configParsing) where

import Data.Text qualified as T
import Home.Reactive.App (Config (..))
import Home.Reactive.MQTT (mqttTopicFilters)
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
