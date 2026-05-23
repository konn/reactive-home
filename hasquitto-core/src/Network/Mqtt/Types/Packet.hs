{- | The MQTT v5 control packet model: one sum over all 15 packet types, with a
record per packet body. This is pure wire vocabulary — no IO, no validation
beyond the types themselves (the codec validates the wire rules).
-}
module Network.Mqtt.Types.Packet (
  -- * The packet sum
  Packet (..),

  -- * Per-packet bodies
  ConnectPacket (..),
  ConnAckPacket (..),
  PublishPacket (..),
  PubAckPacket (..),
  SubscribePacket (..),
  SubAckPacket (..),
  UnsubscribePacket (..),
  DisconnectPacket (..),
  AuthPacket (..),

  -- * Subscriptions
  Subscription (..),
  RetainHandling (..),
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word16)
import Network.Mqtt.Types.PacketId (PacketId)
import Network.Mqtt.Types.Property (Properties)
import Network.Mqtt.Types.QoS (QoS)
import Network.Mqtt.Types.ReasonCode (ReasonCode)
import Network.Mqtt.Types.Topic (TopicFilter)
import Network.Mqtt.Types.Will (Will)

-- | An MQTT v5 control packet.
data Packet
  = Connect !ConnectPacket
  | ConnAck !ConnAckPacket
  | Publish !PublishPacket
  | PubAck !PubAckPacket
  | PubRec !PubAckPacket
  | PubRel !PubAckPacket
  | PubComp !PubAckPacket
  | Subscribe !SubscribePacket
  | SubAck !SubAckPacket
  | Unsubscribe !UnsubscribePacket
  | UnsubAck !SubAckPacket
  | PingReq
  | PingResp
  | Disconnect !DisconnectPacket
  | Auth !AuthPacket
  deriving stock (Show, Eq)

-- | CONNECT (§3.1).
data ConnectPacket = ConnectPacket
  { clientId :: !Text
  -- ^ May be empty to request a server-assigned identifier.
  , cleanStart :: !Bool
  , keepAlive :: !Word16
  -- ^ Keep-alive interval in seconds; @0@ disables it.
  , username :: !(Maybe Text)
  , password :: !(Maybe ByteString)
  , will :: !(Maybe Will)
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | CONNACK (§3.2).
data ConnAckPacket = ConnAckPacket
  { sessionPresent :: !Bool
  , reasonCode :: !ReasonCode
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

{- | PUBLISH (§3.3). The @topic@ is the raw wire Topic Name; it may be empty when
a Topic Alias property is present. @packetId@ is present iff @qos > QoS0@.
-}
data PublishPacket = PublishPacket
  { topic :: !Text
  , packetId :: !(Maybe PacketId)
  , qos :: !QoS
  , retain :: !Bool
  , dup :: !Bool
  , payload :: !ByteString
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

{- | The shared shape of PUBACK\/PUBREC\/PUBREL\/PUBCOMP (§3.4–3.7): a packet
identifier, a reason code, and properties. The compact wire forms (reason code
defaulting to @0x00@, properties omitted) are handled by the codec.
-}
data PubAckPacket = PubAckPacket
  { packetId :: !PacketId
  , reasonCode :: !ReasonCode
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | SUBSCRIBE (§3.8).
data SubscribePacket = SubscribePacket
  { packetId :: !PacketId
  , subscriptions :: !(NonEmpty Subscription)
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | A single entry in a SUBSCRIBE packet, with its subscription options.
data Subscription = Subscription
  { topicFilter :: !TopicFilter
  , qos :: !QoS
  , noLocal :: !Bool
  , retainAsPublished :: !Bool
  , retainHandling :: !RetainHandling
  }
  deriving stock (Show, Eq)

-- | Retain Handling subscription option (§3.8.3.1).
data RetainHandling
  = -- | Send retained messages at subscribe time (@0@).
    SendOnSubscribe
  | -- | Send retained messages only if the subscription did not already exist (@1@).
    SendIfNew
  | -- | Do not send retained messages at subscribe time (@2@).
    DontSend
  deriving stock (Show, Eq, Enum, Bounded)

-- | SUBACK (§3.9); also reused for UNSUBACK (§3.11), whose shape is identical.
data SubAckPacket = SubAckPacket
  { packetId :: !PacketId
  , reasonCodes :: !(NonEmpty ReasonCode)
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | UNSUBSCRIBE (§3.10).
data UnsubscribePacket = UnsubscribePacket
  { packetId :: !PacketId
  , topicFilters :: !(NonEmpty TopicFilter)
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | DISCONNECT (§3.14). An empty wire body means reason @0x00@ and no properties.
data DisconnectPacket = DisconnectPacket
  { reasonCode :: !ReasonCode
  , properties :: !Properties
  }
  deriving stock (Show, Eq)

-- | AUTH (§3.15), used for enhanced authentication.
data AuthPacket = AuthPacket
  { reasonCode :: !ReasonCode
  , properties :: !Properties
  }
  deriving stock (Show, Eq)
