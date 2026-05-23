-- | MQTT packet identifiers.
module Network.Mqtt.Types.PacketId (
  PacketId (..),
) where

import Data.Word (Word16)

{- | A nonzero MQTT packet identifier (@1@ .. @65535@).

The value @0@ is forbidden by the spec (MQTT-2.2.1-3); allocation of identifiers
is the client's responsibility and the codec rejects @0@ where an identifier is
required.
-}
newtype PacketId = PacketId {unPacketId :: Word16}
  deriving stock (Show, Eq, Ord)
