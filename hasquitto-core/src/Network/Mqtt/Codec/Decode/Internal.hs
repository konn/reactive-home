{- | Decoding MQTT v5 packets from a complete byte buffer. Validates fixed-header
flags, packet-identifier nonzero-ness, compact acknowledgement forms, property
value ranges, and single-use property duplication. Negotiated-state checks
(e.g. Topic Alias bounds) are the client layer's responsibility, not this one's.
-}
module Network.Mqtt.Codec.Decode.Internal (
  decodeBody,
  decodeFrame,
  frameParser,
  getProperties,
) where

import Data.Bits (shiftR, testBit, (.&.))
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Word (Word16, Word32, Word8)
import Network.Mqtt.Codec.Parser.Internal
import Network.Mqtt.Codec.Wire.Internal
import Network.Mqtt.Types.Packet
import Network.Mqtt.Types.PacketId (PacketId (..))
import Network.Mqtt.Types.Property
import Network.Mqtt.Types.QoS (QoS (..))
import Network.Mqtt.Types.ReasonCode (ReasonCode (..), pattern NormalDisconnection, pattern Success)
import Network.Mqtt.Types.Topic (Topic, mkTopic, mkTopicFilter)
import Network.Mqtt.Types.Will (Will (..))

-- | Decode a packet from its fixed-header first byte and complete body bytes.
decodeBody :: Word8 -> ByteString -> Either DecodeError Packet
decodeBody hdr body = runComplete (packetParser (hdr `shiftR` 4) (hdr .&. 0x0F)) body

{- | Parse one complete frame (fixed header + remaining length + body) from a
buffer, returning the packet and any trailing bytes.
-}
decodeFrame :: ByteString -> Either DecodeError (Packet, ByteString)
decodeFrame = runParser frameParser

-- | A 'Parser' for one whole frame.
frameParser :: Parser Packet
frameParser = do
  hdr <- getWord8
  rl <- getVarInt
  body <- takeN rl
  either failP pure (decodeBody hdr body)

-- Dispatch ------------------------------------------------------------------

packetParser :: Word8 -> Word8 -> Parser Packet
packetParser ty flags
  | ty == ctConnect = withFlags 0 (Connect <$> decodeConnect)
  | ty == ctConnAck = withFlags 0 (ConnAck <$> decodeConnAck)
  | ty == ctPublish = Publish <$> decodePublish flags
  | ty == ctPubAck = withFlags 0 (PubAck <$> decodeAck pubAckRecReasons)
  | ty == ctPubRec = withFlags 0 (PubRec <$> decodeAck pubAckRecReasons)
  | ty == ctPubRel = withFlags 0b0010 (PubRel <$> decodeAck pubRelCompReasons)
  | ty == ctPubComp = withFlags 0 (PubComp <$> decodeAck pubRelCompReasons)
  | ty == ctSubscribe = withFlags 0b0010 (Subscribe <$> decodeSubscribe)
  | ty == ctSubAck = withFlags 0 (SubAck <$> decodeSubAck subAckReasons)
  | ty == ctUnsubscribe = withFlags 0b0010 (Unsubscribe <$> decodeUnsubscribe)
  | ty == ctUnsubAck = withFlags 0 (UnsubAck <$> decodeSubAck unsubAckReasons)
  | ty == ctPingReq = withFlags 0 (pure PingReq)
  | ty == ctPingResp = withFlags 0 (pure PingResp)
  | ty == ctDisconnect = withFlags 0 (Disconnect <$> decodeDisconnect)
  | ty == ctAuth = withFlags 0 (Auth <$> decodeAuth)
  | otherwise = failP (UnknownPacketType ty)
  where
    withFlags expected p
      | flags == expected = p
      | otherwise = failP (InvalidFlags ty flags)

-- Per-packet decoders -------------------------------------------------------

decodeConnect :: Parser ConnectPacket
decodeConnect = do
  proto <- getText
  ver <- getWord8
  flagsByte <- getWord8
  ka <- getWord16be
  props <- getProperties
  allowOnly connectProps props
  cid <- getText
  willMsg <- decodeWill flagsByte
  uname <- if testBit flagsByte 7 then Just <$> getText else pure Nothing
  pass <- if testBit flagsByte 6 then Just <$> getBytes else pure Nothing
  if proto /= "MQTT"
    then failP (Malformed "protocol name")
    else
      if ver /= 5
        then failP (Malformed "protocol version")
        else
          if testBit flagsByte 0
            then failP (Malformed "CONNECT reserved flag set")
            else
              pure
                ConnectPacket
                  { clientId = cid
                  , cleanStart = testBit flagsByte 1
                  , keepAlive = ka
                  , username = uname
                  , password = pass
                  , will = willMsg
                  , properties = props
                  }

decodeWill :: Word8 -> Parser (Maybe Will)
decodeWill flagsByte
  | testBit flagsByte 2 = do
      wprops <- getProperties
      allowOnly willProps wprops
      wtopicRaw <- getText
      wpayload <- getBytes
      qos <- qosFromBits ((flagsByte `shiftR` 3) .&. 0x03)
      top <- topic wtopicRaw
      pure $
        Just
          Will
            { topic = top
            , payload = wpayload
            , qos = qos
            , retain = testBit flagsByte 5
            , properties = wprops
            }
  | (flagsByte `shiftR` 3) .&. 0x03 /= 0 || testBit flagsByte 5 =
      -- Will QoS / Will Retain must be 0 when there is no Will (MQTT-3.1.2-11/13).
      failP (Malformed "will flags set without will")
  | otherwise = pure Nothing

decodeConnAck :: Parser ConnAckPacket
decodeConnAck = do
  ackFlags <- getWord8
  rc <- ReasonCode <$> getWord8
  checkReason connackReasons rc
  props <- getProperties
  allowOnly connackProps props
  if ackFlags .&. 0xFE /= 0
    then failP (Malformed "CONNACK reserved flags set")
    else
      pure
        ConnAckPacket
          { sessionPresent = testBit ackFlags 0
          , reasonCode = rc
          , properties = props
          }

decodePublish :: Word8 -> Parser PublishPacket
decodePublish flags = do
  qos <- qosFromBits ((flags `shiftR` 1) .&. 0x03)
  let dup = testBit flags 3
  if qos == QoS0 && dup
    then failP (Malformed "DUP set on QoS0 PUBLISH")
    else do
      topicName <- getText
      pid <- if qos /= QoS0 then Just . PacketId <$> nonZeroId else pure Nothing
      props <- getProperties
      allowOnly publishProps props
      body <- getRemaining
      pure
        PublishPacket
          { topic = topicName
          , packetId = pid
          , qos = qos
          , retain = testBit flags 0
          , dup = dup
          , payload = body
          , properties = props
          }

{- | The compact PUBACK\/PUBREC\/PUBREL\/PUBCOMP body: RL 2 = id only (reason
@0x00@), RL 3 = id + reason, longer = id + reason + properties.
-}
decodeAck :: [Word8] -> Parser PubAckPacket
decodeAck allowedReasons = do
  pid <- PacketId <$> nonZeroId
  done <- atEnd
  if done
    then pure PubAckPacket {packetId = pid, reasonCode = Success, properties = []}
    else do
      rc <- ReasonCode <$> getWord8
      checkReason allowedReasons rc
      done' <- atEnd
      props <- if done' then pure [] else getProperties
      allowOnly pubAckProps props
      pure PubAckPacket {packetId = pid, reasonCode = rc, properties = props}

decodeSubscribe :: Parser SubscribePacket
decodeSubscribe = do
  pid <- PacketId <$> nonZeroId
  props <- getProperties
  allowOnly subscribeProps props
  -- A SUBSCRIBE may carry at most one Subscription Identifier (MQTT-3.8.2.1.2);
  -- the generic property loop allows it to repeat (legal on forwarded PUBLISH).
  if length (filter ((== pidSubscriptionIdentifier) . propertyId) props) > 1
    then failP (DuplicateProperty pidSubscriptionIdentifier)
    else pure ()
  subs <- parseList1 decodeSubscription
  pure SubscribePacket {packetId = pid, subscriptions = subs, properties = props}

decodeSubscription :: Parser Subscription
decodeSubscription = do
  raw <- getText
  opt <- getWord8
  if opt .&. 0xC0 /= 0
    then failP (Malformed "subscription options reserved bits set")
    else do
      tf <- either (const (failP (Malformed "topic filter"))) pure (mkTopicFilter raw)
      qos <- qosFromBits (opt .&. 0x03)
      rh <- case (opt `shiftR` 4) .&. 0x03 of
        0 -> pure SendOnSubscribe
        1 -> pure SendIfNew
        2 -> pure DontSend
        _ -> failP (Malformed "retain handling")
      pure
        Subscription
          { topicFilter = tf
          , qos = qos
          , noLocal = testBit opt 2
          , retainAsPublished = testBit opt 3
          , retainHandling = rh
          }

decodeSubAck :: [Word8] -> Parser SubAckPacket
decodeSubAck allowedReasons = do
  pid <- PacketId <$> nonZeroId
  props <- getProperties
  allowOnly subAckProps props
  rcs <- parseList1 do
    r <- ReasonCode <$> getWord8
    checkReason allowedReasons r
    pure r
  pure SubAckPacket {packetId = pid, reasonCodes = rcs, properties = props}

decodeUnsubscribe :: Parser UnsubscribePacket
decodeUnsubscribe = do
  pid <- PacketId <$> nonZeroId
  props <- getProperties
  allowOnly unsubscribeProps props
  fs <-
    parseList1 do
      raw <- getText
      either (const (failP (Malformed "topic filter"))) pure (mkTopicFilter raw)
  pure UnsubscribePacket {packetId = pid, topicFilters = fs, properties = props}

decodeDisconnect :: Parser DisconnectPacket
decodeDisconnect = do
  done <- atEnd
  if done
    then pure DisconnectPacket {reasonCode = NormalDisconnection, properties = []}
    else do
      rc <- ReasonCode <$> getWord8
      checkReason disconnectReasons rc
      done' <- atEnd
      props <- if done' then pure [] else getProperties
      allowOnly disconnectProps props
      pure DisconnectPacket {reasonCode = rc, properties = props}

decodeAuth :: Parser AuthPacket
decodeAuth = do
  done <- atEnd
  if done
    then pure AuthPacket {reasonCode = Success, properties = []}
    else do
      rc <- ReasonCode <$> getWord8
      checkReason authReasons rc
      done' <- atEnd
      props <- if done' then pure [] else getProperties
      allowOnly authProps props
      pure AuthPacket {reasonCode = rc, properties = props}

-- Properties ----------------------------------------------------------------

{- | Decode a property block: a VBI length prefix then that many bytes of
properties. Detects duplicated single-use properties and out-of-range values.
-}
getProperties :: Parser Properties
getProperties = do
  len <- getVarInt
  subParse len (loop [] [])
  where
    loop seen acc = do
      done <- atEnd
      if done
        then pure (reverse acc)
        else do
          pid <- getWord8
          prop <- decodeProperty pid
          seen' <- checkDuplicate seen pid
          loop seen' (prop : acc)
    checkDuplicate seen pid
      | pid == pidUserProperty = pure seen
      | pid == pidSubscriptionIdentifier = pure seen
      | pid `elem` seen = failP (DuplicateProperty pid)
      | otherwise = pure (pid : seen)

decodeProperty :: Word8 -> Parser Property
decodeProperty pid
  | pid == pidPayloadFormatIndicator = PayloadFormatIndicator <$> payloadFormat
  | pid == pidMessageExpiryInterval = MessageExpiryInterval <$> getWord32be
  | pid == pidContentType = ContentType <$> getText
  | pid == pidResponseTopic = ResponseTopic <$> (getText >>= topic)
  | pid == pidCorrelationData = CorrelationData <$> getBytes
  | pid == pidSubscriptionIdentifier = SubscriptionIdentifier <$> subscriptionId
  | pid == pidSessionExpiryInterval = SessionExpiryInterval <$> getWord32be
  | pid == pidAssignedClientIdentifier = AssignedClientIdentifier <$> getText
  | pid == pidServerKeepAlive = ServerKeepAlive <$> getWord16be
  | pid == pidAuthenticationMethod = AuthenticationMethod <$> getText
  | pid == pidAuthenticationData = AuthenticationData <$> getBytes
  | pid == pidRequestProblemInformation = RequestProblemInformation <$> boolValue pid
  | pid == pidWillDelayInterval = WillDelayInterval <$> getWord32be
  | pid == pidRequestResponseInformation = RequestResponseInformation <$> boolValue pid
  | pid == pidResponseInformation = ResponseInformation <$> getText
  | pid == pidServerReference = ServerReference <$> getText
  | pid == pidReasonString = ReasonString <$> getText
  | pid == pidReceiveMaximum = ReceiveMaximum <$> nonZero16 pid
  | pid == pidTopicAliasMaximum = TopicAliasMaximum <$> getWord16be
  | pid == pidTopicAlias = TopicAlias <$> nonZero16 pid
  | pid == pidMaximumQoS = MaximumQoS <$> maximumQoS
  | pid == pidRetainAvailable = RetainAvailable <$> boolValue pid
  | pid == pidUserProperty = uncurry UserProperty <$> getStringPair
  | pid == pidMaximumPacketSize = MaximumPacketSize <$> nonZero32 pid
  | pid == pidWildcardSubscriptionAvailable = WildcardSubscriptionAvailable <$> boolValue pid
  | pid == pidSubscriptionIdentifierAvailable = SubscriptionIdentifierAvailable <$> boolValue pid
  | pid == pidSharedSubscriptionAvailable = SharedSubscriptionAvailable <$> boolValue pid
  | otherwise = failP (UnknownProperty pid)

-- Value helpers -------------------------------------------------------------

payloadFormat :: Parser PayloadFormat
payloadFormat =
  getWord8 >>= \case
    0 -> pure Unspecified
    1 -> pure Utf8
    _ -> failP (PropertyValueOutOfRange pidPayloadFormatIndicator)

boolValue :: Word8 -> Parser Bool
boolValue pid =
  getWord8 >>= \case
    0 -> pure False
    1 -> pure True
    _ -> failP (PropertyValueOutOfRange pid)

maximumQoS :: Parser QoS
maximumQoS =
  getWord8 >>= \case
    0 -> pure QoS0
    1 -> pure QoS1
    _ -> failP (PropertyValueOutOfRange pidMaximumQoS)

nonZero16 :: Word8 -> Parser Word16
nonZero16 pid = getWord16be >>= \v -> if v == 0 then failP (PropertyValueOutOfRange pid) else pure v

nonZero32 :: Word8 -> Parser Word32
nonZero32 pid = getWord32be >>= \v -> if v == 0 then failP (PropertyValueOutOfRange pid) else pure v

subscriptionId :: Parser Word32
subscriptionId =
  getVarInt >>= \v ->
    if v == 0
      then failP (PropertyValueOutOfRange pidSubscriptionIdentifier)
      else pure (fromIntegral v)

topic :: Text -> Parser Topic
topic t = either (const (failP (Malformed "topic name"))) pure (mkTopic t)

qosFromBits :: Word8 -> Parser QoS
qosFromBits = \case
  0 -> pure QoS0
  1 -> pure QoS1
  2 -> pure QoS2
  _ -> failP InvalidQoSBits

nonZeroId :: Parser Word16
nonZeroId = getWord16be >>= \w -> if w == 0 then failP PacketIdentifierZero else pure w

-- | Parse a non-empty list, consuming until the buffer is exhausted.
parseList1 :: Parser a -> Parser (NonEmpty a)
parseList1 p = do
  done <- atEnd
  if done
    then failP EmptyList
    else do
      x <- p
      xs <- go []
      pure (x :| reverse xs)
  where
    go acc = do
      done <- atEnd
      if done then pure acc else p >>= \y -> go (y : acc)

-- Per-packet property legality -----------------------------------------------

-- | The wire identifier of a property (its inverse of 'decodeProperty').
propertyId :: Property -> Word8
propertyId = \case
  PayloadFormatIndicator _ -> pidPayloadFormatIndicator
  MessageExpiryInterval _ -> pidMessageExpiryInterval
  ContentType _ -> pidContentType
  ResponseTopic _ -> pidResponseTopic
  CorrelationData _ -> pidCorrelationData
  SubscriptionIdentifier _ -> pidSubscriptionIdentifier
  SessionExpiryInterval _ -> pidSessionExpiryInterval
  AssignedClientIdentifier _ -> pidAssignedClientIdentifier
  ServerKeepAlive _ -> pidServerKeepAlive
  AuthenticationMethod _ -> pidAuthenticationMethod
  AuthenticationData _ -> pidAuthenticationData
  RequestProblemInformation _ -> pidRequestProblemInformation
  WillDelayInterval _ -> pidWillDelayInterval
  RequestResponseInformation _ -> pidRequestResponseInformation
  ResponseInformation _ -> pidResponseInformation
  ServerReference _ -> pidServerReference
  ReasonString _ -> pidReasonString
  ReceiveMaximum _ -> pidReceiveMaximum
  TopicAliasMaximum _ -> pidTopicAliasMaximum
  TopicAlias _ -> pidTopicAlias
  MaximumQoS _ -> pidMaximumQoS
  RetainAvailable _ -> pidRetainAvailable
  UserProperty _ _ -> pidUserProperty
  MaximumPacketSize _ -> pidMaximumPacketSize
  WildcardSubscriptionAvailable _ -> pidWildcardSubscriptionAvailable
  SubscriptionIdentifierAvailable _ -> pidSubscriptionIdentifierAvailable
  SharedSubscriptionAvailable _ -> pidSharedSubscriptionAvailable

-- | Reject any property not in the packet's permitted set (§2.2.2 / §3.x).
allowOnly :: [Word8] -> Properties -> Parser ()
allowOnly allowed = mapM_ check
  where
    check p =
      let i = propertyId p
       in if i `elem` allowed then pure () else failP (PropertyNotAllowed i)

connectProps, willProps, connackProps, publishProps, pubAckProps :: [Word8]
subscribeProps, subAckProps, unsubscribeProps, disconnectProps, authProps :: [Word8]
connectProps =
  [ pidSessionExpiryInterval
  , pidReceiveMaximum
  , pidMaximumPacketSize
  , pidTopicAliasMaximum
  , pidRequestResponseInformation
  , pidRequestProblemInformation
  , pidUserProperty
  , pidAuthenticationMethod
  , pidAuthenticationData
  ]
willProps =
  [ pidWillDelayInterval
  , pidPayloadFormatIndicator
  , pidMessageExpiryInterval
  , pidContentType
  , pidResponseTopic
  , pidCorrelationData
  , pidUserProperty
  ]
connackProps =
  [ pidSessionExpiryInterval
  , pidReceiveMaximum
  , pidMaximumQoS
  , pidRetainAvailable
  , pidMaximumPacketSize
  , pidAssignedClientIdentifier
  , pidTopicAliasMaximum
  , pidReasonString
  , pidUserProperty
  , pidWildcardSubscriptionAvailable
  , pidSubscriptionIdentifierAvailable
  , pidSharedSubscriptionAvailable
  , pidServerKeepAlive
  , pidResponseInformation
  , pidServerReference
  , pidAuthenticationMethod
  , pidAuthenticationData
  ]
publishProps =
  [ pidPayloadFormatIndicator
  , pidMessageExpiryInterval
  , pidTopicAlias
  , pidResponseTopic
  , pidCorrelationData
  , pidUserProperty
  , pidSubscriptionIdentifier
  , pidContentType
  ]
pubAckProps = [pidReasonString, pidUserProperty]

subscribeProps = [pidSubscriptionIdentifier, pidUserProperty]

subAckProps = [pidReasonString, pidUserProperty]

unsubscribeProps = [pidUserProperty]

disconnectProps = [pidSessionExpiryInterval, pidReasonString, pidUserProperty, pidServerReference]

authProps = [pidAuthenticationMethod, pidAuthenticationData, pidReasonString, pidUserProperty]

-- Per-packet reason codes ----------------------------------------------------

{- | Reject a reason code not permitted for the packet (§2.4 tables per packet).
The 'ReasonCode' representation stays a raw 'Word8' (forward-compatible), but
the wire decoder enforces the spec's closed per-packet set.
-}
checkReason :: [Word8] -> ReasonCode -> Parser ()
checkReason allowed (ReasonCode w)
  | w `elem` allowed = pure ()
  | otherwise = failP (InvalidReasonCode w)

connackReasons, pubAckRecReasons, pubRelCompReasons :: [Word8]
subAckReasons, unsubAckReasons, disconnectReasons, authReasons :: [Word8]
connackReasons =
  [ 0x00
  , 0x80
  , 0x81
  , 0x82
  , 0x83
  , 0x84
  , 0x85
  , 0x86
  , 0x87
  , 0x88
  , 0x89
  , 0x8A
  , 0x8C
  , 0x90
  , 0x95
  , 0x97
  , 0x99
  , 0x9A
  , 0x9B
  , 0x9C
  , 0x9D
  , 0x9F
  ]
pubAckRecReasons = [0x00, 0x10, 0x80, 0x83, 0x87, 0x90, 0x91, 0x97, 0x99]
pubRelCompReasons = [0x00, 0x92]

subAckReasons = [0x00, 0x01, 0x02, 0x80, 0x83, 0x87, 0x8F, 0x91, 0x97, 0x9E, 0xA1, 0xA2]

unsubAckReasons = [0x00, 0x11, 0x80, 0x83, 0x87, 0x8F, 0x91]

disconnectReasons =
  [ 0x00
  , 0x04
  , 0x80
  , 0x81
  , 0x82
  , 0x83
  , 0x87
  , 0x89
  , 0x8B
  , 0x8D
  , 0x8E
  , 0x8F
  , 0x90
  , 0x91
  , 0x93
  , 0x94
  , 0x95
  , 0x96
  , 0x97
  , 0x98
  , 0x99
  , 0x9A
  , 0x9B
  , 0x9C
  , 0x9D
  , 0x9E
  , 0x9F
  , 0xA0
  , 0xA1
  , 0xA2
  ]

authReasons = [0x00, 0x18, 0x19]
