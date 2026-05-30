-- | The Will (Last Will and Testament) message carried in a CONNECT packet.
module Network.Mqtt.Types.Will (
  Will (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Network.Mqtt.Types.Property (Properties)
import Network.Mqtt.Types.QoS (QoS)
import Network.Mqtt.Types.Topic (Topic)

{- | A Will message: published by the server on the client's behalf if the
connection ends abnormally (§3.1.3.3). The @properties@ are Will Properties such
as @WillDelayInterval@.
-}
data Will = Will
  { topic :: !Topic
  , payload :: !ByteString
  , qos :: !QoS
  , retain :: !Bool
  , properties :: !Properties
  }
  deriving stock (Show, Eq, Generic)
