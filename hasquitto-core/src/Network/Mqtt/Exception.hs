-- | The exception hierarchy thrown by the connection and client layers.
module Network.Mqtt.Exception (
  MqttException (..),
  ConnectionError (..),
  ProtocolError (..),
) where

import Control.Exception (Exception)
import GHC.Generics (Generic)
import Network.Mqtt.Codec (DecodeError)
import Network.Mqtt.Types.Property (Properties)
import Network.Mqtt.Types.ReasonCode (ReasonCode)

-- | Transport-level conditions.
data ConnectionError
  = -- | The peer closed the connection cleanly between frames.
    ConnectionClosed
  | -- | The connection ended in the middle of a frame (truncated).
    ConnectionClosedMidPacket
  deriving stock (Show, Eq, Generic)

-- | Protocol-level conditions surfaced to the caller.
data ProtocolError
  = -- | The server refused the CONNECT (CONNACK reason @>= 0x80@).
    ConnectionRefused !ReasonCode !Properties
  | -- | The server sent a DISCONNECT.
    ServerDisconnected !ReasonCode !Properties
  | -- | A packet arrived that was not valid for the current state.
    UnexpectedPacket !String
  | -- | No PINGRESP arrived within the keep-alive window.
    KeepAliveExpired
  | -- | A packet exceeded the negotiated Maximum Packet Size.
    OversizePacket !Int
  | -- | Enhanced authentication failed.
    AuthenticationFailed !ReasonCode
  | {- | The requested operation is not supported by the server (a CONNACK
    capability gate, e.g. a QoS above Maximum QoS, retain when not available).
    -}
    UnsupportedByServer !String
  | -- | The CONNECT/CONNACK handshake did not complete in time.
    ConnectTimedOut
  deriving stock (Show, Eq, Generic)

-- | The top-level exception type for this library.
data MqttException
  = -- | A packet could not be decoded.
    DecodeFailed !DecodeError
  | -- | A protocol rule was violated (by us or the server).
    ProtocolViolation !ProtocolError
  | -- | The transport failed or closed.
    TransportClosed !ConnectionError
  deriving stock (Show, Generic)

instance Exception MqttException
