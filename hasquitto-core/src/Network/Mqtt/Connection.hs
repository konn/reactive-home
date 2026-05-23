{- | The MQTT connection abstraction: a swappable byte transport plus the packet
framing built on top of it. The default TCP backend lives in
"Network.Mqtt.Connection.TCP"; TLS or WebSocket transports can be plugged in by
supplying a different 'Connection'.
-}
module Network.Mqtt.Connection (
  -- * Raw transport
  Connection (..),

  -- * Buffered connection
  Conn,
  makeConnection,
  fromConnection,
  connectionUnread,
  connectionReadExactly,

  -- * Framing
  MaxPacketSize,
  defaultMaxPacketSize,
  readPacket,
  writePacket,
) where

import Network.Mqtt.Connection.Internal (
  Conn,
  Connection (..),
  MaxPacketSize,
  connectionReadExactly,
  connectionUnread,
  defaultMaxPacketSize,
  fromConnection,
  makeConnection,
  readPacket,
  writePacket,
 )
