{- | Unit tests for the 'Mqtt' effect plumbing, with no broker.

A stub interpreter records the name of every operation it dispatches (and returns canned
values), so we can assert that each smart constructor sends the matching 'Mqtt' constructor.
The whole suite is pure 'Eff' (no 'IOE'): 'getClient' returns a never-forced thunk, and the
acquisition helpers ('connect' \/ 'withClient' \/ 'runMqtt' \/ 'runMqttWith') are 'IOE'-bound
and exercised by the integration suite instead.
-}
module Main (main) where

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Effectful (Eff, runPureEff, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Network.Mqtt
import Effectful.State.Static.Local (State, execState, modify)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain (testGroup "hasquitto-effectful" [dispatchTests])

dispatchTests :: TestTree
dispatchTests =
  testGroup
    "Mqtt effect dispatch"
    [ testCase "every smart constructor dispatches its operation, in order" $
        runPureEff (execState [] (runStub program)) @?= expected
    ]
  where
    expected :: [String]
    expected =
      [ "getClient"
      , "getSession"
      , "isConnected"
      , "status"
      , "subscribe"
      , "subscribe1"
      , "publish_"
      , "publish"
      , "subscriptions"
      , "recvMessage"
      , "tryRecvMessage"
      , "unsubscribe"
      , "ping"
      ]

-- | A small program that exercises each operation exactly once, in a known order.
program :: (Mqtt :> es) => Eff es ()
program = do
  _ <- getClient
  _ <- getSession
  _ <- isConnected
  _ <- status
  _ <- subscribe (sub :| []) []
  _ <- subscribe1 filt QoS1
  publish_ top "x"
  _ <- publish top "y" defaultPublishOptions
  _ <- subscriptions
  _ <- recvMessage
  _ <- tryRecvMessage
  _ <- unsubscribe (filt :| []) []
  ping
  where
    top = either (error . show) id (mkTopic "test/topic")
    filt = either (error . show) id (mkTopicFilter "test/#")
    sub =
      Subscription
        { topicFilter = filt
        , qos = QoS1
        , noLocal = False
        , retainAsPublished = False
        , retainHandling = SendOnSubscribe
        }

{- | Interpret 'Mqtt' purely: append each operation's name to the 'State' log and return a
canned value. 'GetClient' returns a never-forced thunk (the stub holds no real client).
-}
runStub :: (State [String] :> es) => Eff (Mqtt : es) a -> Eff es a
runStub = interpret_ \case
  GetClient -> record "getClient" $> noClient
  GetSession -> record "getSession" $> dummySession
  Status -> record "status" $> Connected
  IsConnected -> record "isConnected" $> True
  Publish {} -> record "publish" $> PublishedQoS0
  Publish_ {} -> record "publish_"
  Subscribe subs _ -> record "subscribe" $> fmap (const Success) subs
  Subscribe1 {} -> record "subscribe1" $> Success
  Unsubscribe fs _ -> record "unsubscribe" $> fmap (const Success) fs
  Ping -> record "ping"
  Subscriptions -> record "subscriptions" $> []
  RecvMessage -> record "recvMessage" $> dummyMessage
  TryRecvMessage -> record "tryRecvMessage" $> Just dummyMessage
  where
    noClient = error "AutoClient must not be forced by the stub interpreter" :: AutoClient
    dummySession =
      Session
        { sessionPresent = False
        , reasonCode = Success
        , assignedClientId = Nothing
        , serverProperties = []
        }
    dummyMessage =
      Message
        { topic = either (error . show) id (mkTopic "test/topic")
        , payload = ""
        , qos = QoS0
        , retain = False
        , dup = False
        , properties = []
        }

record :: (State [String] :> es) => String -> Eff es ()
record name = modify (<> [name])
