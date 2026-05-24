module Network.Mqtt.Types.TopicTest (test_topicMatching) where

import Data.Text (Text)
import Network.Mqtt.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_topicMatching :: TestTree
test_topicMatching =
  testGroup
    "topic matching"
    [ testCase "single-level wildcard matches one level" $
        matchesText "sport/+/player1" "sport/tennis/player1" @?= True
    , testCase "single-level wildcard does not span levels" $
        matchesText "sport/+" "sport/tennis/player1" @?= False
    , testCase "multi-level wildcard matches the parent" $
        matchesText "sport/#" "sport" @?= True
    , testCase "multi-level wildcard matches deeper" $
        matchesText "sport/#" "sport/tennis/player1" @?= True
    , testCase "wildcards do not match $-topics" $
        matchesText "#" "$SYS/broker/uptime" @?= False
    ]

matchesText :: Text -> Text -> Bool
matchesText f t =
  case (mkTopicFilter f, mkTopic t) of
    (Right tf, Right top) -> matches tf top
    _ -> error "invalid test topic/filter"
