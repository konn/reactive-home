{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Reactive.ESPresenseTest (test_tomlParsing) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Home.Reactive.ESPresense (
  ESPSensor (..),
  ESPresenseConfig (..),
  Room (..),
  RoomSensor (..),
  espresenseConfigCodec,
  minutes,
  seconds,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Toml (decodeExact)

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
