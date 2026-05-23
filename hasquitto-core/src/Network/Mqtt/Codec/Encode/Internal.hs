{- | Encoding MQTT v5 packets to a 'Builder'. Total and faithful: it serialises
whatever 'Packet' it is given (validity is the caller's concern), and emits the
shortest legal form of the compact acknowledgement packets.
-}
module Network.Mqtt.Codec.Encode.Internal (
  -- * Whole packets
  encodePacket,
  encodePacketLazy,
  encodePacketBS,

  -- * Primitive emitters
  putVarInt,
  putText,
  putBytes,
  putStringPair,
  putProperties,
  putProperty,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word16BE, word32BE, word8)
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word8)
import Network.Mqtt.Codec.Wire.Internal
import Network.Mqtt.Types.Packet
import Network.Mqtt.Types.PacketId (PacketId (..))
import Network.Mqtt.Types.Property
import Network.Mqtt.Types.QoS (QoS (..))
import Network.Mqtt.Types.ReasonCode (ReasonCode (..))
import Network.Mqtt.Types.Topic (Topic (..), TopicFilter (..))
import Network.Mqtt.Types.Will (Will (..))

-- | Encode a packet (fixed header + remaining length + body) as a 'Builder'.
encodePacket :: Packet -> Builder
encodePacket pkt =
  word8 ((ty `shiftL` 4) .|. flags)
    <> putVarInt (BS.length body)
    <> byteString body
  where
    (ty, flags) = headerBits pkt
    body = strict (encodeBody pkt)

-- | Encode a packet to a lazy 'LBS.ByteString'.
encodePacketLazy :: Packet -> LBS.ByteString
encodePacketLazy = toLazyByteString . encodePacket

-- | Encode a packet to a strict 'ByteString'.
encodePacketBS :: Packet -> ByteString
encodePacketBS = LBS.toStrict . encodePacketLazy

strict :: Builder -> ByteString
strict = LBS.toStrict . toLazyByteString

-- | Fixed-header type nibble and flags nibble for a packet.
headerBits :: Packet -> (Word8, Word8)
headerBits = \case
  Connect _ -> (ctConnect, 0)
  ConnAck _ -> (ctConnAck, 0)
  Publish p ->
    ( ctPublish
    , (boolBit p.dup `shiftL` 3) .|. (qosBits p.qos `shiftL` 1) .|. boolBit p.retain
    )
  PubAck _ -> (ctPubAck, 0)
  PubRec _ -> (ctPubRec, 0)
  PubRel _ -> (ctPubRel, 0b0010)
  PubComp _ -> (ctPubComp, 0)
  Subscribe _ -> (ctSubscribe, 0b0010)
  SubAck _ -> (ctSubAck, 0)
  Unsubscribe _ -> (ctUnsubscribe, 0b0010)
  UnsubAck _ -> (ctUnsubAck, 0)
  PingReq -> (ctPingReq, 0)
  PingResp -> (ctPingResp, 0)
  Disconnect _ -> (ctDisconnect, 0)
  Auth _ -> (ctAuth, 0)

-- | The variable header + payload of a packet (no fixed header).
encodeBody :: Packet -> Builder
encodeBody = \case
  Connect c -> encodeConnect c
  ConnAck c -> encodeConnAck c
  Publish p -> encodePublish p
  PubAck p -> encodeAck p
  PubRec p -> encodeAck p
  PubRel p -> encodeAck p
  PubComp p -> encodeAck p
  Subscribe s -> encodeSubscribe s
  SubAck s -> encodeSubAck s
  Unsubscribe u -> encodeUnsubscribe u
  UnsubAck s -> encodeSubAck s
  PingReq -> mempty
  PingResp -> mempty
  Disconnect d -> encodeReasonBody d.reasonCode d.properties
  Auth a -> encodeReasonBody a.reasonCode a.properties

encodeConnect :: ConnectPacket -> Builder
encodeConnect c =
  putText "MQTT"
    <> word8 5
    <> word8 connectFlags
    <> word16BE c.keepAlive
    <> putProperties c.properties
    <> putText c.clientId
    <> willPart
    <> maybe mempty putText c.username
    <> maybe mempty putBytes c.password
  where
    connectFlags =
      maybe 0 (const 0x80) c.username
        .|. maybe 0 (const 0x40) c.password
        .|. willFlags
        .|. (if c.cleanStart then 0x02 else 0)
    (willFlags, willPart) = encodeWill c.will

encodeWill :: Maybe Will -> (Word8, Builder)
encodeWill Nothing = (0, mempty)
encodeWill (Just w) =
  ( 0x04 .|. (qosBits w.qos `shiftL` 3) .|. (if w.retain then 0x20 else 0)
  , putProperties w.properties <> putText w.topic.raw <> putBytes w.payload
  )

encodeConnAck :: ConnAckPacket -> Builder
encodeConnAck c =
  word8 (if c.sessionPresent then 1 else 0)
    <> word8 (reasonByte c.reasonCode)
    <> putProperties c.properties

encodePublish :: PublishPacket -> Builder
encodePublish p =
  putText p.topic
    <> idPart
    <> putProperties p.properties
    <> byteString p.payload
  where
    idPart = case p.packetId of
      Just (PacketId i) | p.qos /= QoS0 -> word16BE i
      _ -> mempty

-- | The shortest legal form of a PUBACK\/PUBREC\/PUBREL\/PUBCOMP body (§3.4.2.1).
encodeAck :: PubAckPacket -> Builder
encodeAck p = case (null p.properties, p.reasonCode) of
  (True, ReasonCode 0x00) -> word16BE (unPid p.packetId)
  (True, _) -> word16BE (unPid p.packetId) <> word8 (reasonByte p.reasonCode)
  (False, _) ->
    word16BE (unPid p.packetId)
      <> word8 (reasonByte p.reasonCode)
      <> putProperties p.properties

-- | The shortest legal DISCONNECT\/AUTH body: an empty body means reason @0x00@.
encodeReasonBody :: ReasonCode -> Properties -> Builder
encodeReasonBody reason props = case (null props, reason) of
  (True, ReasonCode 0x00) -> mempty
  (True, _) -> word8 (reasonByte reason)
  (False, _) -> word8 (reasonByte reason) <> putProperties props

encodeSubscribe :: SubscribePacket -> Builder
encodeSubscribe s =
  word16BE (unPid s.packetId)
    <> putProperties s.properties
    <> foldMap putSub (NE.toList s.subscriptions)
  where
    putSub sub = putText sub.topicFilter.raw <> word8 (optionsByte sub)
    optionsByte sub =
      qosBits sub.qos
        .|. (if sub.noLocal then 0x04 else 0)
        .|. (if sub.retainAsPublished then 0x08 else 0)
        .|. (fromIntegral (fromEnum sub.retainHandling) `shiftL` 4)

encodeSubAck :: SubAckPacket -> Builder
encodeSubAck s =
  word16BE (unPid s.packetId)
    <> putProperties s.properties
    <> foldMap (word8 . reasonByte) (NE.toList s.reasonCodes)

encodeUnsubscribe :: UnsubscribePacket -> Builder
encodeUnsubscribe u =
  word16BE (unPid u.packetId)
    <> putProperties u.properties
    <> foldMap (\(TopicFilter t) -> putText t) (NE.toList u.topicFilters)

-- Primitive emitters --------------------------------------------------------

-- | Emit a Variable Byte Integer. Assumes @0 <= n <= 268435455@.
putVarInt :: Int -> Builder
putVarInt = go
  where
    go n
      | n < 0x80 = word8 (fromIntegral n)
      | otherwise = word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> go (n `shiftR` 7)

{- | Emit a UTF-8 Encoded String (16-bit length prefix + UTF-8 bytes). The MQTT
maximum is 65535 bytes; a longer value is a precondition violation (it cannot
be represented on the wire) and raises an error rather than silently truncating.
-}
putText :: Text -> Builder
putText t = lengthPrefixed "UTF-8 string" (TE.encodeUtf8 t)

{- | Emit Binary Data (16-bit length prefix + bytes). See 'putText' for the
65535-byte limit.
-}
putBytes :: ByteString -> Builder
putBytes = lengthPrefixed "binary data"

lengthPrefixed :: String -> ByteString -> Builder
lengthPrefixed what bs
  | n > 0xFFFF = error ("Network.Mqtt.Codec: " <> what <> " exceeds 65535 bytes (" <> show n <> ")")
  | otherwise = word16BE (fromIntegral n) <> byteString bs
  where
    n = BS.length bs

-- | Emit a UTF-8 String Pair.
putStringPair :: Text -> Text -> Builder
putStringPair k v = putText k <> putText v

-- | Emit a property block: a VBI length prefix then the encoded properties.
putProperties :: Properties -> Builder
putProperties props =
  let body = strict (foldMap putProperty props)
   in putVarInt (BS.length body) <> byteString body

-- | Emit a single property (identifier + typed value).
putProperty :: Property -> Builder
putProperty = \case
  PayloadFormatIndicator f -> word8 pidPayloadFormatIndicator <> word8 (fromIntegral (fromEnum f))
  MessageExpiryInterval v -> word8 pidMessageExpiryInterval <> word32BE v
  ContentType t -> word8 pidContentType <> putText t
  ResponseTopic (Topic t) -> word8 pidResponseTopic <> putText t
  CorrelationData b -> word8 pidCorrelationData <> putBytes b
  SubscriptionIdentifier v -> word8 pidSubscriptionIdentifier <> putVarInt (fromIntegral v)
  SessionExpiryInterval v -> word8 pidSessionExpiryInterval <> word32BE v
  AssignedClientIdentifier t -> word8 pidAssignedClientIdentifier <> putText t
  ServerKeepAlive v -> word8 pidServerKeepAlive <> word16BE v
  AuthenticationMethod t -> word8 pidAuthenticationMethod <> putText t
  AuthenticationData b -> word8 pidAuthenticationData <> putBytes b
  RequestProblemInformation b -> word8 pidRequestProblemInformation <> boolByte b
  WillDelayInterval v -> word8 pidWillDelayInterval <> word32BE v
  RequestResponseInformation b -> word8 pidRequestResponseInformation <> boolByte b
  ResponseInformation t -> word8 pidResponseInformation <> putText t
  ServerReference t -> word8 pidServerReference <> putText t
  ReasonString t -> word8 pidReasonString <> putText t
  ReceiveMaximum v -> word8 pidReceiveMaximum <> word16BE v
  TopicAliasMaximum v -> word8 pidTopicAliasMaximum <> word16BE v
  TopicAlias v -> word8 pidTopicAlias <> word16BE v
  MaximumQoS q -> word8 pidMaximumQoS <> word8 (qosBits q)
  RetainAvailable b -> word8 pidRetainAvailable <> boolByte b
  UserProperty k v -> word8 pidUserProperty <> putStringPair k v
  MaximumPacketSize v -> word8 pidMaximumPacketSize <> word32BE v
  WildcardSubscriptionAvailable b -> word8 pidWildcardSubscriptionAvailable <> boolByte b
  SubscriptionIdentifierAvailable b -> word8 pidSubscriptionIdentifierAvailable <> boolByte b
  SharedSubscriptionAvailable b -> word8 pidSharedSubscriptionAvailable <> boolByte b

-- Small helpers -------------------------------------------------------------

reasonByte :: ReasonCode -> Word8
reasonByte (ReasonCode w) = w

unPid :: PacketId -> Word16
unPid (PacketId i) = i

qosBits :: QoS -> Word8
qosBits = fromIntegral . fromEnum

boolBit :: Bool -> Word8
boolBit True = 1
boolBit False = 0

boolByte :: Bool -> Builder
boolByte = word8 . boolBit
