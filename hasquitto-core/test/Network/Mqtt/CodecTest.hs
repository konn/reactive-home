module Network.Mqtt.CodecTest (
  test_compactForms,
  test_negativeCases,
  test_roundTrips,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word8)
import Network.Mqtt.Codec
import Network.Mqtt.Types
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Property qualified as F
import Test.Falsify.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

test_roundTrips :: TestTree
test_roundTrips =
  testGroup
    "codec round-trips"
    [testProperty "decodeFrame . encodePacketBS == id (every packet)" roundtripProperty]

roundtripProperty :: F.Property ()
roundtripProperty = do
  pkt <- F.gen genPacket
  F.assert $
    P.eq
      .$ ("expected", Right (pkt, BS.empty))
      .$ ("actual", decodeFrame (encodePacketBS pkt))

test_compactForms :: TestTree
test_compactForms =
  testGroup
    "compact acknowledgement forms decode"
    [ testCase "PUBACK remaining-length 2 means reason Success, no properties" $
        decodeFrame (BS.pack [0x40, 0x02, 0x00, 0x05])
          @?= Right (PubAck (PubAckPacket (PacketId 5) Success []), BS.empty)
    , testCase "empty DISCONNECT body means NormalDisconnection" $
        decodeFrame (BS.pack [0xE0, 0x00])
          @?= Right (Disconnect (DisconnectPacket NormalDisconnection []), BS.empty)
    , testCase "empty AUTH body means Success" $
        decodeFrame (BS.pack [0xF0, 0x00])
          @?= Right (Auth (AuthPacket Success []), BS.empty)
    ]

test_negativeCases :: TestTree
test_negativeCases =
  testGroup
    "negative golden cases (must be rejected)"
    [ testCase "PUBREL with wrong fixed-header flags" $
        shouldFail (BS.pack [0x60, 0x02, 0x00, 0x01])
    , testCase "non-minimal Variable Byte Integer (remaining length)" $
        shouldFail (BS.pack [0xC0, 0x80, 0x00])
    , testCase "packet identifier zero in PUBACK" $
        shouldFail (BS.pack [0x40, 0x02, 0x00, 0x00])
    , testCase "Maximum QoS property value 2 in CONNACK" $
        shouldFail (BS.pack [0x20, 0x05, 0x00, 0x00, 0x02, 0x24, 0x02])
    , testCase "duplicate single-use property in CONNACK" $
        shouldFail (BS.pack [0x20, 0x09, 0x00, 0x00, 0x06, 0x21, 0x00, 0x0A, 0x21, 0x00, 0x0B])
    , testCase "PUBLISH with QoS bits 0b11" $
        shouldFail (BS.pack [0x36, 0x02, 0x00, 0x00])
    , testCase "property not allowed on packet (PayloadFormatIndicator in CONNACK)" $
        shouldFail (BS.pack [0x20, 0x05, 0x00, 0x00, 0x02, 0x01, 0x01])
    , testCase "reason code not valid for packet (GrantedQoS1 in PUBACK)" $
        shouldFail (BS.pack [0x40, 0x04, 0x00, 0x05, 0x01, 0x00])
    ]

shouldFail :: ByteString -> IO ()
shouldFail bs = case decodeFrame bs of
  Left _ -> pure ()
  Right r -> assertFailure ("expected decode failure, got: " <> show r)

-- Generators ----------------------------------------------------------------

genPacket :: Gen Packet
genPacket =
  Gen.oneof $
    (Connect <$> genConnect)
      :| [ ConnAck <$> genConnAck
         , Publish <$> genPublish
         , PubAck <$> genAck pubAckRecReasons
         , PubRec <$> genAck pubAckRecReasons
         , PubRel <$> genAck pubRelCompReasons
         , PubComp <$> genAck pubRelCompReasons
         , Subscribe <$> genSubscribe
         , SubAck <$> genSubAck subAckReasons
         , Unsubscribe <$> genUnsubscribe
         , UnsubAck <$> genSubAck unsubAckReasons
         , pure PingReq
         , pure PingResp
         , Disconnect <$> genDisconnect
         , Auth <$> genAuth
         ]

genConnect :: Gen ConnectPacket
genConnect =
  ConnectPacket
    <$> genName
    <*> genBool
    <*> genWord16
    <*> genMaybe genText
    <*> genMaybe genBytes
    <*> genMaybe genWill
    <*> genPropsFrom connectGens

genWill :: Gen Will
genWill = Will <$> genTopic <*> genBytes <*> genQoS <*> genBool <*> genPropsFrom willGens

genConnAck :: Gen ConnAckPacket
genConnAck = ConnAckPacket <$> genBool <*> genReason connackReasons <*> genPropsFrom connackGens

genPublish :: Gen PublishPacket
genPublish = do
  q <- genQoS
  d <- if q == QoS0 then pure False else genBool
  r <- genBool
  pid <- if q == QoS0 then pure Nothing else Just <$> genPacketId
  props <- genPropsFrom publishGens
  body <- genBytes
  t <- genName
  pure PublishPacket {topic = t, packetId = pid, qos = q, retain = r, dup = d, payload = body, properties = props}

genAck :: [Word8] -> Gen PubAckPacket
genAck reasons = PubAckPacket <$> genPacketId <*> genReason reasons <*> genPropsFrom pubAckGens

genSubscribe :: Gen SubscribePacket
genSubscribe = SubscribePacket <$> genPacketId <*> genNonEmpty genSubscription <*> genPropsFrom subscribeGens

genSubscription :: Gen Subscription
genSubscription =
  Subscription
    <$> genTopicFilter
    <*> genQoS
    <*> genBool
    <*> genBool
    <*> Gen.elem (SendOnSubscribe :| [SendIfNew, DontSend])

genSubAck :: [Word8] -> Gen SubAckPacket
genSubAck reasons = SubAckPacket <$> genPacketId <*> genNonEmpty (genReason reasons) <*> genPropsFrom subAckGens

genUnsubscribe :: Gen UnsubscribePacket
genUnsubscribe = UnsubscribePacket <$> genPacketId <*> genNonEmpty genTopicFilter <*> genPropsFrom unsubscribeGens

genDisconnect :: Gen DisconnectPacket
genDisconnect = DisconnectPacket <$> genReason disconnectReasons <*> genPropsFrom disconnectGens

genAuth :: Gen AuthPacket
genAuth = AuthPacket <$> genReason authReasons <*> genPropsFrom authGens

-- Primitive generators ------------------------------------------------------

genBool :: Gen Bool
genBool = Gen.bool False

genWord8 :: Gen Word8
genWord8 = Gen.inRange (Range.between (0, 255))

genWord16 :: Gen Word16
genWord16 = Gen.inRange (Range.between (0, 65535))

genReason :: [Word8] -> Gen ReasonCode
genReason reasons = ReasonCode <$> Gen.elem (toNonEmpty reasons)

genPacketId :: Gen PacketId
genPacketId = PacketId <$> Gen.inRange (Range.between (1, 65535))

genQoS :: Gen QoS
genQoS = Gen.elem (QoS0 :| [QoS1, QoS2])

genName :: Gen Text
genName = T.intercalate "/" <$> Gen.list (Range.between (1, 4)) genLevel
  where
    genLevel = T.pack <$> Gen.list (Range.between (1, 5)) (Gen.elem ('a' :| ['b' .. 'z']))

genTopic :: Gen Topic
genTopic = do
  n <- genName
  either (const (error "bad topic gen")) pure (mkTopic n)

genTopicFilter :: Gen TopicFilter
genTopicFilter = do
  levels <- Gen.list (Range.between (1, 4)) genLevel
  hashTail <- Gen.elem ([] :| [["#"]])
  either (const (error "bad filter gen")) pure (mkTopicFilter (T.intercalate "/" (levels <> hashTail)))
  where
    genLevel =
      Gen.choose
        (T.pack <$> Gen.list (Range.between (1, 5)) (Gen.elem ('a' :| ['b' .. 'z'])))
        (pure "+")

genText :: Gen Text
genText = T.pack <$> Gen.list (Range.between (0, 8)) (Gen.elem (toNonEmpty (['a' .. 'z'] <> ['0' .. '9'] <> " -_.")))

genBytes :: Gen ByteString
genBytes = BS.pack <$> Gen.list (Range.between (0, 16)) genWord8

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = Gen.choose (pure Nothing) (Just <$> g)

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = (:|) <$> g <*> Gen.list (Range.between (0, 4)) g

toNonEmpty :: [a] -> NonEmpty a
toNonEmpty (x : xs) = x :| xs
toNonEmpty [] = error "toNonEmpty: empty"

{- | A property list using only generators legal for the packet (each single-use
one at most once), plus zero or more user properties (legal everywhere).
-}
genPropsFrom :: [Gen Property] -> Gen Properties
genPropsFrom gens = do
  singles <- concat <$> mapM (\g -> Gen.choose (pure []) ((: []) <$> g)) gens
  users <- Gen.list (Range.between (0, 2)) (UserProperty <$> genText <*> genText)
  pure (singles <> users)

genWord32 :: Gen Word32
genWord32 = Gen.inRange (Range.between (0, 4294967295))

-- Per-packet legal property generators (mirroring the decoder's allowed sets).

connectGens, willGens, connackGens, publishGens, pubAckGens :: [Gen Property]
subscribeGens, subAckGens, unsubscribeGens, disconnectGens, authGens :: [Gen Property]
connectGens =
  [ SessionExpiryInterval <$> genWord32
  , ReceiveMaximum <$> Gen.inRange (Range.between (1, 65535))
  , MaximumPacketSize <$> Gen.inRange (Range.between (1, 4294967295))
  , TopicAliasMaximum <$> genWord16
  , RequestResponseInformation <$> genBool
  , RequestProblemInformation <$> genBool
  , AuthenticationMethod <$> genText
  , AuthenticationData <$> genBytes
  ]
willGens =
  [ WillDelayInterval <$> genWord32
  , PayloadFormatIndicator <$> Gen.elem (Unspecified :| [Utf8])
  , MessageExpiryInterval <$> genWord32
  , ContentType <$> genText
  , ResponseTopic <$> genTopic
  , CorrelationData <$> genBytes
  ]
connackGens =
  [ SessionExpiryInterval <$> genWord32
  , ReceiveMaximum <$> Gen.inRange (Range.between (1, 65535))
  , MaximumQoS <$> Gen.elem (QoS0 :| [QoS1])
  , RetainAvailable <$> genBool
  , MaximumPacketSize <$> Gen.inRange (Range.between (1, 4294967295))
  , AssignedClientIdentifier <$> genText
  , TopicAliasMaximum <$> genWord16
  , ReasonString <$> genText
  , WildcardSubscriptionAvailable <$> genBool
  , SubscriptionIdentifierAvailable <$> genBool
  , SharedSubscriptionAvailable <$> genBool
  , ServerKeepAlive <$> genWord16
  , ResponseInformation <$> genText
  , ServerReference <$> genText
  , AuthenticationMethod <$> genText
  , AuthenticationData <$> genBytes
  ]
publishGens =
  [ PayloadFormatIndicator <$> Gen.elem (Unspecified :| [Utf8])
  , MessageExpiryInterval <$> genWord32
  , TopicAlias <$> Gen.inRange (Range.between (1, 65535))
  , ResponseTopic <$> genTopic
  , CorrelationData <$> genBytes
  , SubscriptionIdentifier <$> Gen.inRange (Range.between (1, 268435455))
  , ContentType <$> genText
  ]
pubAckGens = [ReasonString <$> genText]

subscribeGens = [SubscriptionIdentifier <$> Gen.inRange (Range.between (1, 268435455))]

subAckGens = [ReasonString <$> genText]

unsubscribeGens = []

disconnectGens =
  [ SessionExpiryInterval <$> genWord32
  , ReasonString <$> genText
  , ServerReference <$> genText
  ]

authGens =
  [ AuthenticationMethod <$> genText
  , AuthenticationData <$> genBytes
  , ReasonString <$> genText
  ]

-- Per-packet legal reason codes (mirroring the decoder).

pubAckRecReasons, pubRelCompReasons, subAckReasons :: [Word8]
unsubAckReasons, connackReasons, disconnectReasons, authReasons :: [Word8]
pubAckRecReasons = [0x00, 0x10, 0x80, 0x83, 0x87, 0x90, 0x91, 0x97, 0x99]
pubRelCompReasons = [0x00, 0x92]
subAckReasons = [0x00, 0x01, 0x02, 0x80, 0x83, 0x87, 0x8F, 0x91, 0x97, 0x9E, 0xA1, 0xA2]

unsubAckReasons = [0x00, 0x11, 0x80, 0x83, 0x87, 0x8F, 0x91]

connackReasons =
  [0x00, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8C, 0x90, 0x95, 0x97, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9F]

disconnectReasons =
  [0x00, 0x04, 0x80, 0x81, 0x82, 0x83, 0x87, 0x89, 0x8B, 0x8D, 0x8E, 0x8F, 0x90, 0x91, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, 0xA0, 0xA1, 0xA2]

authReasons = [0x00, 0x18, 0x19]
