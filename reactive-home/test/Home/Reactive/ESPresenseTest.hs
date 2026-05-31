{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Home.Reactive.ESPresenseTest (test_tomlParsing) where

import Data.Text qualified as T
import Home.Reactive.ESPresense (ESPSensor (..), ESPresenseConfig (..), seconds)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Toml (decodeExact, genericCodec)

test_tomlParsing :: TestTree
test_tomlParsing =
  testGroup
    "TOML parsing"
    [ testCase "uses ESPRoom defaults only when optional keys are absent" $
        decodeExact (genericCodec @ESPresenseConfig) minimalRoomToml
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
                      }
                  ]
              }
    , testCase "keeps present ESPRoom values instead of replacing them with defaults" $
        decodeExact (genericCodec @ESPresenseConfig) explicitRoomToml
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
                      }
                  ]
              }
    , testCase "parses real example TOML correctly" $
        decodeExact (genericCodec @ESPresenseConfig) realExampleToml
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
                      }
                  , ESPSensor
                      { name = "bedroom"
                      , max_distance = 8
                      , skip_distance = 0.5
                      , skip_ms = 5000
                      , timeout = seconds 5.5
                      }
                  ]
              }
    , testCase "does not hide invalid present ESPRoom values behind defaults" $
        case decodeExact (genericCodec @ESPresenseConfig) invalidRoomToml of
          Left _ -> pure ()
          Right cfg -> assertFailure $ "expected TOML decode failure, got: " <> show cfg
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
