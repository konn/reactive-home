{- | The connection abstraction (an http-client-style read\/write\/close record)
with an internal leftover buffer, plus the packet framing that reads one whole
frame and hands its body to the pure decoder.
-}
module Network.Mqtt.Connection.Internal (
  -- * Raw transport
  Connection (..),

  -- * Buffered connection
  Conn (..),
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

import Control.Exception (throwIO)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Network.Mqtt.Codec (DecodeError (..), decodeBody, encodePacketBS)
import Network.Mqtt.Exception (ConnectionError (..), MqttException (..), ProtocolError (..))
import Network.Mqtt.Types.Packet (Packet)

{- | A raw bidirectional byte transport. 'connectionRead' returns @""@ at
end-of-input. This is the swappable abstraction: a TLS or WebSocket backend
produces one of these just as the TCP backend does.
-}
data Connection = Connection
  { connectionRead :: !(IO ByteString)
  , connectionWrite :: !(ByteString -> IO ())
  , connectionClose :: !(IO ())
  }

{- | A 'Connection' plus a single-reader leftover buffer used for pushback during
framing. Only one thread (the reader) should read from a 'Conn'; the buffer is
therefore an ordinary 'IORef' with no locking.
-}
data Conn = Conn
  { base :: !Connection
  , leftover :: !(IORef ByteString)
  }

-- | Build a buffered connection from raw read\/write\/close actions.
makeConnection :: IO ByteString -> (ByteString -> IO ()) -> IO () -> IO Conn
makeConnection r w c = fromConnection (Connection r w c)

-- | Wrap an existing raw 'Connection' with a fresh leftover buffer.
fromConnection :: Connection -> IO Conn
fromConnection conn = Conn conn <$> newIORef BS.empty

-- | Read the next chunk, draining the leftover buffer first. @""@ means EOF.
rawRead :: Conn -> IO ByteString
rawRead c = do
  buf <- readIORef c.leftover
  if BS.null buf
    then c.base.connectionRead
    else writeIORef c.leftover BS.empty >> pure buf

-- | Push bytes back to be read again next.
connectionUnread :: Conn -> ByteString -> IO ()
connectionUnread c bs
  | BS.null bs = pure ()
  | otherwise = modifyIORef' c.leftover (bs <>)

-- | Read one byte; 'Nothing' on a clean EOF (used only between frames).
readByteMaybe :: Conn -> IO (Maybe Word8)
readByteMaybe c = do
  chunk <- rawRead c
  case BS.uncons chunk of
    Nothing -> pure Nothing
    Just (b, rest) -> connectionUnread c rest >> pure (Just b)

-- | Read one byte, throwing 'ConnectionClosedMidPacket' on EOF.
readByteMid :: Conn -> IO Word8
readByteMid c =
  readByteMaybe c >>= maybe (throwIO (TransportClosed ConnectionClosedMidPacket)) pure

{- | Read exactly @n@ bytes, accumulating chunks and pushing back any surplus.
Throws 'ConnectionClosedMidPacket' if EOF arrives first. @n == 0@ reads nothing.
-}
connectionReadExactly :: Conn -> Int -> IO ByteString
connectionReadExactly c = go []
  where
    go acc 0 = pure (BS.concat (reverse acc))
    go acc k = do
      chunk <- rawRead c
      if BS.null chunk
        then throwIO (TransportClosed ConnectionClosedMidPacket)
        else do
          let (h, t) = BS.splitAt k chunk
              taken = BS.length h
          if taken == k
            then connectionUnread c t >> pure (BS.concat (reverse (h : acc)))
            else go (h : acc) (k - taken)

{- | A cap on the total size (fixed header + remaining length + body) of an
inbound packet, in bytes. Enforced before allocating the body buffer.
-}
type MaxPacketSize = Int

{- | A generous default cap (the largest a Variable Byte Integer can express plus
its header), used until the server advertises a smaller Maximum Packet Size.
-}
defaultMaxPacketSize :: MaxPacketSize
defaultMaxPacketSize = 268435455 + 5

{- | Read one complete frame and decode it. Distinguishes a clean EOF between
frames ('ConnectionClosed') from a truncated frame ('ConnectionClosedMidPacket'),
and enforces @maxSize@ before allocating the body.
-}
readPacket :: MaxPacketSize -> Conn -> IO Packet
readPacket maxSize c = do
  mhdr <- readByteMaybe c
  case mhdr of
    Nothing -> throwIO (TransportClosed ConnectionClosed)
    Just hdr -> do
      rl <- readRemainingLength c
      let total = 1 + varIntWidth rl + rl
      if total > maxSize
        then throwIO (ProtocolViolation (OversizePacket total))
        else do
          body <- connectionReadExactly c rl
          either (throwIO . DecodeFailed) pure (decodeBody hdr body)

-- | Read the Remaining Length Variable Byte Integer directly off the transport.
readRemainingLength :: Conn -> IO Int
readRemainingLength c = go 0 0
  where
    go acc count = do
      b <- readByteMid c
      let acc' = acc .|. ((fromIntegral (b .&. 0x7F)) `shiftL` (7 * count))
          count' = count + 1
      if b .&. 0x80 /= 0
        then
          if count' >= 4
            then throwIO (DecodeFailed MalformedVarInt)
            else go acc' count'
        else
          if count > 0 && b == 0
            then throwIO (DecodeFailed MalformedVarInt)
            else pure acc'

varIntWidth :: Int -> Int
varIntWidth n
  | n < 0x80 = 1
  | n < 0x4000 = 2
  | n < 0x200000 = 3
  | otherwise = 4

-- | Write a packet to the transport as a single chunk.
writePacket :: Conn -> Packet -> IO ()
writePacket c pkt = c.base.connectionWrite (encodePacketBS pkt)
