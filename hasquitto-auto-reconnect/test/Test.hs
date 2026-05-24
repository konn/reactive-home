-- | White-box unit\/property tests for the pure helpers of the auto-reconnect wrapper.
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Network.Mqtt.Client.AutoReconnect.Internal (
  BackoffConfig (..),
  backoffBound,
  defaultSubscription,
  recordSubscriptions,
  removeSubscriptions,
 )
import Network.Mqtt.Types (QoS (..), ReasonCode (..), Subscription (..), TopicFilter (..))
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Property qualified as F
import Test.Falsify.Range qualified as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain (testGroup "hasquitto-auto-reconnect" [backoffTests, registryTests])

-- Backoff -------------------------------------------------------------------

backoffTests :: TestTree
backoffTests =
  testGroup
    "full-jitter backoff bound"
    [ testProperty "always within [0, max 0 maxDelay] for any inputs" prop_inRange
    , testProperty "equals base * 2^(attempt-1) when unsaturated" prop_exact
    , testCase "attempt 1 yields the base delay" $
        backoffBound (BackoffConfig 100 1_000_000) 1 @?= 100
    , testCase "grows exponentially (attempt 4 = base * 2^3)" $
        backoffBound (BackoffConfig 100 1_000_000) 4 @?= 800
    , testCase "is clamped to the cap" $
        backoffBound (BackoffConfig 100 250) 4 @?= 250
    , testCase "huge base does not overflow and stays capped" $
        backoffBound (BackoffConfig maxBound 30_000_000) 1000 @?= 30_000_000
    , testCase "negative base and cap clamp to 0" $
        backoffBound (BackoffConfig (-5) (-5)) 3 @?= 0
    , testCase "attempt <= 0 behaves like attempt 1" $
        backoffBound (BackoffConfig 100 1_000_000) 0 @?= 100
    ]

-- The bound is always non-negative and never exceeds the (non-negative) cap, no
-- matter how large or negative the inputs are — i.e. it never overflows Int.
prop_inRange :: F.Property ()
prop_inRange = do
  base <- F.gen (anInt (minBound, maxBound))
  cap <- F.gen (anInt (minBound, maxBound))
  attempt <- F.gen (anInt (-5, 5000))
  let b = backoffBound (BackoffConfig base cap) attempt
  F.assert $
    P.eq
      .$ ("expected", True)
      .$ ("actual (0 <= bound <= max 0 cap)", 0 <= b && b <= max 0 cap)

-- In the unsaturated regime (small base and attempt, generous cap) the bound is
-- exactly @base * 2^(attempt-1)@.
prop_exact :: F.Property ()
prop_exact = do
  base <- F.gen (anInt (0, 1000))
  attempt <- F.gen (anInt (1, 20))
  let raw = base * (2 ^ (attempt - 1)) -- base <= 1000, 2^19 < 2^20: well within Int
      cap = raw + 1
  F.assert $
    P.eq
      .$ ("expected", raw)
      .$ ("actual", backoffBound (BackoffConfig base cap) attempt)

anInt :: (Int, Int) -> Gen Int
anInt (lo, hi) = Gen.inRange (Range.between (lo, hi))

-- Registry ------------------------------------------------------------------

registryTests :: TestTree
registryTests =
  testGroup
    "subscription registry helpers"
    [ testCase "recordSubscriptions keeps only successful filters" $
        Map.keys (recordSubscriptions [(sub tf1, ok), (sub tf2, bad)] Map.empty) @?= [tf1]
    , testCase "recordSubscriptions overwrites an existing filter" $
        let m = recordSubscriptions [(sub tf1, ok)] Map.empty
            m' = recordSubscriptions [(subAt tf1 QoS2, ok)] m
         in fmap (.qos) (Map.lookup tf1 m') @?= Just QoS2
    , testCase "removeSubscriptions drops only successfully-unsubscribed filters" $
        let m0 = recordSubscriptions [(sub tf1, ok), (sub tf2, ok)] Map.empty
         in Map.keys (removeSubscriptions [(tf1, ok), (tf2, bad)] m0) @?= [tf2]
    , testCase "defaultSubscription uses the requested QoS and plain options" $ do
        let s = defaultSubscription tf1 QoS2
        s.qos @?= QoS2
        s.noLocal @?= False
        s.retainAsPublished @?= False
    ]
  where
    tf1, tf2 :: TopicFilter
    tf1 = TopicFilter "home/a"
    tf2 = TopicFilter "home/b"
    sub :: TopicFilter -> Subscription
    sub tf = subAt tf QoS1
    subAt :: TopicFilter -> QoS -> Subscription
    subAt = defaultSubscription
    ok, bad :: ReasonCode
    ok = ReasonCode (0x01 :: Word8) -- Granted QoS 1
    bad = ReasonCode (0x80 :: Word8) -- Unspecified error
