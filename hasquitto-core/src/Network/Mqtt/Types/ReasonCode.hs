-- | MQTT v5 reason codes (§2.4) and their named values.
module Network.Mqtt.Types.ReasonCode (
  ReasonCode (..),
  isSuccess,
  isError,

  -- * Named values
  -- $patterns
  pattern Success,
  pattern NormalDisconnection,
  pattern GrantedQoS0,
  pattern GrantedQoS1,
  pattern GrantedQoS2,
  pattern DisconnectWithWillMessage,
  pattern NoMatchingSubscribers,
  pattern NoSubscriptionExisted,
  pattern ContinueAuthentication,
  pattern ReAuthenticate,
  pattern UnspecifiedError,
  pattern MalformedPacket,
  pattern ProtocolError,
  pattern ImplementationSpecificError,
  pattern UnsupportedProtocolVersion,
  pattern ClientIdentifierNotValid,
  pattern BadUserNameOrPassword,
  pattern NotAuthorized,
  pattern ServerUnavailable,
  pattern ServerBusy,
  pattern Banned,
  pattern ServerShuttingDown,
  pattern BadAuthenticationMethod,
  pattern KeepAliveTimeout,
  pattern SessionTakenOver,
  pattern TopicFilterInvalid,
  pattern TopicNameInvalid,
  pattern PacketIdentifierInUse,
  pattern PacketIdentifierNotFound,
  pattern ReceiveMaximumExceeded,
  pattern TopicAliasInvalid,
  pattern PacketTooLarge,
  pattern MessageRateTooHigh,
  pattern QuotaExceeded,
  pattern AdministrativeAction,
  pattern PayloadFormatInvalid,
  pattern RetainNotSupported,
  pattern QoSNotSupported,
  pattern UseAnotherServer,
  pattern ServerMoved,
  pattern SharedSubscriptionsNotSupported,
  pattern ConnectionRateExceeded,
  pattern MaximumConnectTime,
  pattern SubscriptionIdentifiersNotSupported,
  pattern WildcardSubscriptionsNotSupported,
) where

import Data.Word (Word8)

{- | A reason code is a single byte. We keep the raw representation (rather than a
closed enumeration) so decoding is total and forward-compatible; named values are
provided as pattern synonyms. A value @< 0x80@ is a success/normal code, @>= 0x80@
is an error (§2.4).
-}
newtype ReasonCode = ReasonCode {unReasonCode :: Word8}
  deriving stock (Show, Eq, Ord)

-- | Is this a success/normal reason code (@< 0x80@)?
isSuccess :: ReasonCode -> Bool
isSuccess (ReasonCode w) = w < 0x80

-- | Is this an error reason code (@>= 0x80@)?
isError :: ReasonCode -> Bool
isError = not . isSuccess

{- $patterns
The byte @0x00@ is named @Success@, @NormalDisconnection@, and @GrantedQoS0@
depending on the packet it appears in; all three patterns denote the same value.
-}

pattern Success :: ReasonCode
pattern Success = ReasonCode 0x00

pattern NormalDisconnection :: ReasonCode
pattern NormalDisconnection = ReasonCode 0x00

pattern GrantedQoS0 :: ReasonCode
pattern GrantedQoS0 = ReasonCode 0x00

pattern GrantedQoS1 :: ReasonCode
pattern GrantedQoS1 = ReasonCode 0x01

pattern GrantedQoS2 :: ReasonCode
pattern GrantedQoS2 = ReasonCode 0x02

pattern DisconnectWithWillMessage :: ReasonCode
pattern DisconnectWithWillMessage = ReasonCode 0x04

pattern NoMatchingSubscribers :: ReasonCode
pattern NoMatchingSubscribers = ReasonCode 0x10

pattern NoSubscriptionExisted :: ReasonCode
pattern NoSubscriptionExisted = ReasonCode 0x11

pattern ContinueAuthentication :: ReasonCode
pattern ContinueAuthentication = ReasonCode 0x18

pattern ReAuthenticate :: ReasonCode
pattern ReAuthenticate = ReasonCode 0x19

pattern UnspecifiedError :: ReasonCode
pattern UnspecifiedError = ReasonCode 0x80

pattern MalformedPacket :: ReasonCode
pattern MalformedPacket = ReasonCode 0x81

pattern ProtocolError :: ReasonCode
pattern ProtocolError = ReasonCode 0x82

pattern ImplementationSpecificError :: ReasonCode
pattern ImplementationSpecificError = ReasonCode 0x83

pattern UnsupportedProtocolVersion :: ReasonCode
pattern UnsupportedProtocolVersion = ReasonCode 0x84

pattern ClientIdentifierNotValid :: ReasonCode
pattern ClientIdentifierNotValid = ReasonCode 0x85

pattern BadUserNameOrPassword :: ReasonCode
pattern BadUserNameOrPassword = ReasonCode 0x86

pattern NotAuthorized :: ReasonCode
pattern NotAuthorized = ReasonCode 0x87

pattern ServerUnavailable :: ReasonCode
pattern ServerUnavailable = ReasonCode 0x88

pattern ServerBusy :: ReasonCode
pattern ServerBusy = ReasonCode 0x89

pattern Banned :: ReasonCode
pattern Banned = ReasonCode 0x8A

pattern ServerShuttingDown :: ReasonCode
pattern ServerShuttingDown = ReasonCode 0x8B

pattern BadAuthenticationMethod :: ReasonCode
pattern BadAuthenticationMethod = ReasonCode 0x8C

pattern KeepAliveTimeout :: ReasonCode
pattern KeepAliveTimeout = ReasonCode 0x8D

pattern SessionTakenOver :: ReasonCode
pattern SessionTakenOver = ReasonCode 0x8E

pattern TopicFilterInvalid :: ReasonCode
pattern TopicFilterInvalid = ReasonCode 0x8F

pattern TopicNameInvalid :: ReasonCode
pattern TopicNameInvalid = ReasonCode 0x90

pattern PacketIdentifierInUse :: ReasonCode
pattern PacketIdentifierInUse = ReasonCode 0x91

pattern PacketIdentifierNotFound :: ReasonCode
pattern PacketIdentifierNotFound = ReasonCode 0x92

pattern ReceiveMaximumExceeded :: ReasonCode
pattern ReceiveMaximumExceeded = ReasonCode 0x93

pattern TopicAliasInvalid :: ReasonCode
pattern TopicAliasInvalid = ReasonCode 0x94

pattern PacketTooLarge :: ReasonCode
pattern PacketTooLarge = ReasonCode 0x95

pattern MessageRateTooHigh :: ReasonCode
pattern MessageRateTooHigh = ReasonCode 0x96

pattern QuotaExceeded :: ReasonCode
pattern QuotaExceeded = ReasonCode 0x97

pattern AdministrativeAction :: ReasonCode
pattern AdministrativeAction = ReasonCode 0x98

pattern PayloadFormatInvalid :: ReasonCode
pattern PayloadFormatInvalid = ReasonCode 0x99

pattern RetainNotSupported :: ReasonCode
pattern RetainNotSupported = ReasonCode 0x9A

pattern QoSNotSupported :: ReasonCode
pattern QoSNotSupported = ReasonCode 0x9B

pattern UseAnotherServer :: ReasonCode
pattern UseAnotherServer = ReasonCode 0x9C

pattern ServerMoved :: ReasonCode
pattern ServerMoved = ReasonCode 0x9D

pattern SharedSubscriptionsNotSupported :: ReasonCode
pattern SharedSubscriptionsNotSupported = ReasonCode 0x9E

pattern ConnectionRateExceeded :: ReasonCode
pattern ConnectionRateExceeded = ReasonCode 0x9F

pattern MaximumConnectTime :: ReasonCode
pattern MaximumConnectTime = ReasonCode 0xA0

pattern SubscriptionIdentifiersNotSupported :: ReasonCode
pattern SubscriptionIdentifiersNotSupported = ReasonCode 0xA1

pattern WildcardSubscriptionsNotSupported :: ReasonCode
pattern WildcardSubscriptionsNotSupported = ReasonCode 0xA2
