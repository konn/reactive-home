{- | Shared wire constants: MQTT v5 property identifiers and control-packet
fixed-header bytes. Single source of truth for the encoder and decoder.
-}
module Network.Mqtt.Codec.Wire.Internal (
  -- * Property identifiers (§2.2.2.2)
  pidPayloadFormatIndicator,
  pidMessageExpiryInterval,
  pidContentType,
  pidResponseTopic,
  pidCorrelationData,
  pidSubscriptionIdentifier,
  pidSessionExpiryInterval,
  pidAssignedClientIdentifier,
  pidServerKeepAlive,
  pidAuthenticationMethod,
  pidAuthenticationData,
  pidRequestProblemInformation,
  pidWillDelayInterval,
  pidRequestResponseInformation,
  pidResponseInformation,
  pidServerReference,
  pidReasonString,
  pidReceiveMaximum,
  pidTopicAliasMaximum,
  pidTopicAlias,
  pidMaximumQoS,
  pidRetainAvailable,
  pidUserProperty,
  pidMaximumPacketSize,
  pidWildcardSubscriptionAvailable,
  pidSubscriptionIdentifierAvailable,
  pidSharedSubscriptionAvailable,

  -- * Control packet type nibbles (§2.1.2)
  ctConnect,
  ctConnAck,
  ctPublish,
  ctPubAck,
  ctPubRec,
  ctPubRel,
  ctPubComp,
  ctSubscribe,
  ctSubAck,
  ctUnsubscribe,
  ctUnsubAck,
  ctPingReq,
  ctPingResp,
  ctDisconnect,
  ctAuth,
) where

import Data.Word (Word8)

pidPayloadFormatIndicator
  , pidMessageExpiryInterval
  , pidContentType
  , pidResponseTopic
  , pidCorrelationData
  , pidSubscriptionIdentifier
  , pidSessionExpiryInterval
  , pidAssignedClientIdentifier
  , pidServerKeepAlive
  , pidAuthenticationMethod
  , pidAuthenticationData
  , pidRequestProblemInformation
  , pidWillDelayInterval
  , pidRequestResponseInformation
  , pidResponseInformation
  , pidServerReference
  , pidReasonString
  , pidReceiveMaximum
  , pidTopicAliasMaximum
  , pidTopicAlias
  , pidMaximumQoS
  , pidRetainAvailable
  , pidUserProperty
  , pidMaximumPacketSize
  , pidWildcardSubscriptionAvailable
  , pidSubscriptionIdentifierAvailable
  , pidSharedSubscriptionAvailable ::
    Word8
pidPayloadFormatIndicator = 0x01
pidMessageExpiryInterval = 0x02
pidContentType = 0x03
pidResponseTopic = 0x08
pidCorrelationData = 0x09
pidSubscriptionIdentifier = 0x0B
pidSessionExpiryInterval = 0x11
pidAssignedClientIdentifier = 0x12
pidServerKeepAlive = 0x13
pidAuthenticationMethod = 0x15
pidAuthenticationData = 0x16
pidRequestProblemInformation = 0x17
pidWillDelayInterval = 0x18
pidRequestResponseInformation = 0x19
pidResponseInformation = 0x1A
pidServerReference = 0x1C
pidReasonString = 0x1F
pidReceiveMaximum = 0x21
pidTopicAliasMaximum = 0x22
pidTopicAlias = 0x23
pidMaximumQoS = 0x24
pidRetainAvailable = 0x25
pidUserProperty = 0x26
pidMaximumPacketSize = 0x27
pidWildcardSubscriptionAvailable = 0x28
pidSubscriptionIdentifierAvailable = 0x29
pidSharedSubscriptionAvailable = 0x2A

ctConnect
  , ctConnAck
  , ctPublish
  , ctPubAck
  , ctPubRec
  , ctPubRel
  , ctPubComp
  , ctSubscribe
  , ctSubAck
  , ctUnsubscribe
  , ctUnsubAck
  , ctPingReq
  , ctPingResp
  , ctDisconnect
  , ctAuth ::
    Word8
ctConnect = 1
ctConnAck = 2
ctPublish = 3
ctPubAck = 4
ctPubRec = 5
ctPubRel = 6
ctPubComp = 7
ctSubscribe = 8
ctSubAck = 9
ctUnsubscribe = 10
ctUnsubAck = 11
ctPingReq = 12
ctPingResp = 13
ctDisconnect = 14
ctAuth = 15
