module Network.Mqtt.Types.ReasonCodeTest (test_reasonCodeShow) where

import Network.Mqtt.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

test_reasonCodeShow :: TestTree
test_reasonCodeShow =
  testGroup
    "reason code Show"
    [ testCase "known reason codes use their pattern synonym names" $
        show ProtocolError @?= "ProtocolError"
    , testCase "0x00 uses the first declared name" $
        show Success @?= "Success"
    , testCase "unknown reason codes keep the raw constructor form" $
        show (ReasonCode 0x03) @?= "ReasonCode {unReasonCode = 3}"
    ]
