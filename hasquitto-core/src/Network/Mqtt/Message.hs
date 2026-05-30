-- | A received application message, delivered to the consumer by the client.
module Network.Mqtt.Message (
  Message (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Network.Mqtt.Types.Property (Properties)
import Network.Mqtt.Types.QoS (QoS)
import Network.Mqtt.Types.Topic (Topic)

{- | A message received from the broker. The 'topic' is fully resolved (any Topic
Alias has been expanded). Acknowledgement (PUBACK\/PUBCOMP for QoS 1\/2) is
performed by the client when the message is consumed, so that the broker's
Receive Maximum provides backpressure.
-}
data Message = Message
  { topic :: !Topic
  , payload :: !ByteString
  , qos :: !QoS
  , retain :: !Bool
  , dup :: !Bool
  , properties :: !Properties
  }
  deriving stock (Show, Eq, Generic)
