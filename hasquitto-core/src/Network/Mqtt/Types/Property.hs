-- | MQTT v5 properties (§2.2.2), the typed metadata attached to most packets.
module Network.Mqtt.Types.Property (
  Property (..),
  PayloadFormat (..),
  Properties,

  -- * Helpers
  userProperties,
  findProperty,
) where

import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Network.Mqtt.Types.QoS (QoS)
import Network.Mqtt.Types.Topic (Topic)

{- | The payload format indicator value (property @0x01@): whether a PUBLISH
payload is unspecified bytes or a UTF-8 encoded string.
-}
data PayloadFormat = Unspecified | Utf8
  deriving stock (Show, Eq, Enum, Bounded, Generic)

{- | A single MQTT v5 property. This is the complete, closed set defined by the
specification; per-packet legality (which properties may appear on which packet)
is enforced by the codec, not by this type.
-}
data Property
  = PayloadFormatIndicator !PayloadFormat
  | MessageExpiryInterval !Word32
  | ContentType !Text
  | ResponseTopic !Topic
  | CorrelationData !ByteString
  | -- | Subscription identifier (a Variable Byte Integer, @1@ .. @268435455@).
    SubscriptionIdentifier !Word32
  | SessionExpiryInterval !Word32
  | AssignedClientIdentifier !Text
  | ServerKeepAlive !Word16
  | AuthenticationMethod !Text
  | AuthenticationData !ByteString
  | RequestProblemInformation !Bool
  | WillDelayInterval !Word32
  | RequestResponseInformation !Bool
  | ResponseInformation !Text
  | ServerReference !Text
  | ReasonString !Text
  | ReceiveMaximum !Word16
  | TopicAliasMaximum !Word16
  | TopicAlias !Word16
  | MaximumQoS !QoS
  | RetainAvailable !Bool
  | {- | A user property: a name/value UTF-8 string pair. May appear multiple
    times and the order is significant.
    -}
    UserProperty !Text !Text
  | MaximumPacketSize !Word32
  | WildcardSubscriptionAvailable !Bool
  | SubscriptionIdentifierAvailable !Bool
  | SharedSubscriptionAvailable !Bool
  deriving stock (Show, Eq, Generic)

-- | A property collection, in wire order. User properties may repeat.
type Properties = [Property]

-- | Extract all user-property name/value pairs, preserving order.
userProperties :: Properties -> [(Text, Text)]
userProperties = mapMaybe \case
  UserProperty k v -> Just (k, v)
  _ -> Nothing

{- | Find the first property matching a projection, e.g.
@findProperty (\\case ServerKeepAlive k -> Just k; _ -> Nothing) props@.
-}
findProperty :: (Property -> Maybe a) -> Properties -> Maybe a
findProperty f = go
  where
    go [] = Nothing
    go (p : ps) = maybe (go ps) Just (f p)
