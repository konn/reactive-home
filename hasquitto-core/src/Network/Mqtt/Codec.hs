{- | The pure MQTT v5 wire codec: encode a 'Packet' to bytes, decode bytes to a
'Packet'. No IO and no streaming-library dependency.
-}
module Network.Mqtt.Codec (
  -- * Encoding
  encodePacket,
  encodePacketLazy,
  encodePacketBS,

  -- * Decoding
  decodeFrame,
  decodeBody,
  DecodeError (..),
) where

import Network.Mqtt.Codec.Decode.Internal (decodeBody, decodeFrame)
import Network.Mqtt.Codec.Encode.Internal (encodePacket, encodePacketBS, encodePacketLazy)
import Network.Mqtt.Codec.Parser.Internal (DecodeError (..))
