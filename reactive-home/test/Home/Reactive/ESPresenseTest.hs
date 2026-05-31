{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Reactive.ESPresenseTest (test_tomlParsing) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Home.Reactive.ESPresense (
  ESPSensor (..),
  ESPresenseConfig (..),
  Room (..),
  SensorCondition (..),
  espresenseConfigCodec,
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
    , testCase "parses rooms with leave/entry conditions" $
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
                  ]
              , rooms =
                  HM.fromList
                    [
                      ( "home"
                      , Room
                          { devices = ["watch:"]
                          , leave =
                              [ SensorCondition {name = "entrance", distance = 6.5}
                              , SensorCondition {name = "bedroom", distance = 5}
                              ]
                          , entry = [SensorCondition {name = "entrance", distance = 5}]
                          }
                      )
                    ]
              }
    ]

minimalRoomToml :: T.Text
minimalRoomToml =
  T.unlines
    [ "devices = []"
    , ""
    , "[[sensors]]"
    , "name = \"office\""
    ]

explicitRoomToml :: T.Text
explicitRoomToml =
  T.unlines
    [ "devices = []"
    , ""
    , "[[sensors]]"
    , "name = \"office\""
    , "max_distance = 3.25"
    , "skip_distance = 1.25"
    , "skip_ms = 1234"
    ]

invalidRoomToml :: T.Text
invalidRoomToml =
  T.unlines
    [ "devices = []"
    , ""
    , "[[sensors]]"
    , "name = \"office\""
    , "max_distance = \"far\""
    ]

realExampleToml :: T.Text
realExampleToml =
  T.unlines
    [ "devices = [\"watch:\"]"
    , ""
    , "[[sensors]]"
    , "name = \"room\""
    , "max_distance = 8"
    , "skip_distance = 0.5"
    , "skip_ms = 5000"
    , ""
    , "[[sensors]]"
    , "name = \"bedroom\""
    , "max_distance = 8"
    , "skip_distance = 0.5"
    , "skip_ms = 5000"
    , "timeout = \"5.5s\""
    ]

roomsToml :: T.Text
roomsToml =
  T.unlines
    [ "devices = [\"watch:\"]"
    , ""
    , "[[sensors]]"
    , "name = \"entrance\""
    , ""
    , "[rooms.home]"
    , "devices = [\"watch:\"]"
    , ""
    , "[rooms.home.leave]"
    , "conditions = [{ name = \"entrance\", distance = 6.5 }, { name = \"bedroom\", distance = 5 }]"
    , ""
    , "[rooms.home.entry]"
    , "conditions = [{ name = \"entrance\", distance = 5 }]"
    ]
