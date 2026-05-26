{- | Unit tests for the 'Mqtt' effect plumbing, with no broker.

A stub interpreter records the name of every operation it dispatches (and returns canned
values), so we can assert that each smart constructor sends the matching 'Mqtt' constructor
and that the higher-order 'withClient' runs its continuation in order. The whole suite is
pure 'Eff' (no 'IOE'): the opaque 'AutoClient' is passed as a never-forced thunk that the
stub ignores.
-}
module Main (main) where

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Effectful (Eff, runPureEff, type (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
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
      [ "connect"
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
      , "withClient"
      , "waitClosed"
      , "disconnect"
      ]

-- | A small program that exercises each operation exactly once, in a known order.
program :: (Mqtt :> es) => Eff es ()
program = do
  (c, _) <- connect opts cfg
  _ <- isConnected c
  _ <- status c
  _ <- subscribe c (sub :| []) []
  _ <- subscribe1 c filt QoS1
  publish_ c top "x"
  _ <- publish c top "y" defaultPublishOptions
  _ <- subscriptions c
  _ <- recvMessage c
  _ <- tryRecvMessage c
  _ <- unsubscribe c (filt :| []) []
  ping c
  withClient opts cfg \c' _ -> do
    _ <- waitClosed c'
    disconnect c'
  where
    -- None of these are forced by the stub; the factory is never run either.
    opts = defaultConnectOptions (error "connection factory must not be forced") "test-client"
    cfg = defaultAutoReconnectConfig
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
canned value. The 'AutoClient' argument is ignored (never forced).
-}
runStub :: (State [String] :> es) => Eff (Mqtt : es) a -> Eff es a
runStub = interpret \env -> \case
  Connect _ _ -> record "connect" $> (noClient, dummySession)
  WithClient _ _ k -> do
    record "withClient"
    localSeqUnlift env \unlift -> unlift (k noClient dummySession)
  Disconnect _ -> record "disconnect"
  WaitClosed _ -> record "waitClosed" $> Right Success
  Status _ -> record "status" $> Connected
  IsConnected _ -> record "isConnected" $> True
  Publish {} -> record "publish" $> PublishedQoS0
  Publish_ {} -> record "publish_"
  Subscribe _ subs _ -> record "subscribe" $> fmap (const Success) subs
  Subscribe1 {} -> record "subscribe1" $> Success
  Unsubscribe _ fs _ -> record "unsubscribe" $> fmap (const Success) fs
  Ping _ -> record "ping"
  Subscriptions _ -> record "subscriptions" $> []
  RecvMessage _ -> record "recvMessage" $> dummyMessage
  TryRecvMessage _ -> record "tryRecvMessage" $> Just dummyMessage
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
