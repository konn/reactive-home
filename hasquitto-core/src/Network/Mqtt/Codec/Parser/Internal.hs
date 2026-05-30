{- | A small, total, hand-rolled parser over a complete strict 'ByteString'.

This is deliberately not a streaming parser: MQTT framing lets us read one
whole packet body before parsing, so the parser always works on a fully
materialised buffer and \"ran out of input\" is a hard error, never a request
for more bytes. This keeps the package free of any streaming-library
dependency while giving typed, total decode errors.
-}
module Network.Mqtt.Codec.Parser.Internal (
  -- * The parser
  Parser (..),
  DecodeError (..),
  runParser,
  runComplete,
  failP,

  -- * Primitives
  takeN,
  getWord8,
  getWord16be,
  getWord32be,
  getVarInt,
  getText,
  getBytes,
  getStringPair,
  getRemaining,
  atEnd,
  subParse,

  -- * Variable Byte Integer helpers
  varIntWidth,
) where

import Control.Applicative (Alternative (..))
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)

-- | What can go wrong while decoding a packet.
data DecodeError
  = -- | The parser needed more bytes than the buffer held.
    UnexpectedEndOfInput
  | -- | Bytes remained after a parser that should have consumed everything.
    TrailingBytes !Int
  | -- | A Variable Byte Integer ran past 4 bytes or was non-minimally encoded.
    MalformedVarInt
  | -- | A UTF-8 string was not valid UTF-8.
    InvalidUtf8
  | -- | The fixed-header type nibble was @0@ (reserved) or otherwise unknown.
    UnknownPacketType !Word8
  | {- | The fixed-header flags nibble was illegal for the packet type
    (type nibble, flags nibble).
    -}
    InvalidFlags !Word8 !Word8
  | -- | The PUBLISH QoS bits were @0b11@.
    InvalidQoSBits
  | -- | An unknown property identifier.
    UnknownProperty !Word8
  | -- | A single-use property appeared more than once.
    DuplicateProperty !Word8
  | -- | A property carried a value outside its permitted range.
    PropertyValueOutOfRange !Word8
  | -- | A property identifier was not legal for the packet it appeared on.
    PropertyNotAllowed !Word8
  | -- | A packet identifier was @0@ where a nonzero one is required.
    PacketIdentifierZero
  | -- | A reason code was not valid for the packet it appeared on.
    InvalidReasonCode !Word8
  | -- | A SUBSCRIBE\/UNSUBSCRIBE\/SUBACK list was empty.
    EmptyList
  | -- | A catch-all with context.
    Malformed !String
  deriving stock (Show, Eq, Generic)

{- | A parser threads the remaining input and either fails with a typed
'DecodeError' or yields a value plus the unconsumed bytes.
-}
newtype Parser a = Parser (ByteString -> Either DecodeError (a, ByteString))
  deriving stock (Generic)

-- | Run a parser over a buffer, yielding the result and the unconsumed bytes.
runParser :: Parser a -> ByteString -> Either DecodeError (a, ByteString)
runParser (Parser f) = f
{-# INLINE runParser #-}

instance Functor Parser where
  fmap f (Parser p) = Parser \bs -> case p bs of
    Left e -> Left e
    Right (a, rest) -> Right (f a, rest)
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser \bs -> Right (a, bs)
  {-# INLINE pure #-}
  Parser pf <*> Parser pa = Parser \bs -> case pf bs of
    Left e -> Left e
    Right (f, rest) -> case pa rest of
      Left e -> Left e
      Right (a, rest') -> Right (f a, rest')
  {-# INLINE (<*>) #-}

instance Monad Parser where
  Parser p >>= k = Parser \bs -> case p bs of
    Left e -> Left e
    Right (a, rest) -> runParser (k a) rest
  {-# INLINE (>>=) #-}

{- | 'Alternative' tries the left parser, then the right on failure (without
backtracking cost beyond re-running on the same input).
-}
instance Alternative Parser where
  empty = failP (Malformed "empty")
  Parser p <|> Parser q = Parser \bs -> case p bs of
    Left _ -> q bs
    r -> r

-- | Fail with a specific decode error.
failP :: DecodeError -> Parser a
failP e = Parser \_ -> Left e
{-# INLINE failP #-}

{- | Run a parser and require that it consumes the /entire/ input; leftover bytes
are a 'TrailingBytes' error. This is the terminal runner used on a complete
packet body.
-}
runComplete :: Parser a -> ByteString -> Either DecodeError a
runComplete (Parser p) bs = case p bs of
  Left e -> Left e
  Right (a, rest)
    | BS.null rest -> Right a
    | otherwise -> Left (TrailingBytes (BS.length rest))

-- | Take exactly @n@ bytes, or fail with 'UnexpectedEndOfInput'.
takeN :: Int -> Parser ByteString
takeN n
  | n < 0 = failP (Malformed "negative length")
  | otherwise = Parser \bs ->
      if BS.length bs < n
        then Left UnexpectedEndOfInput
        else Right (BS.splitAt n bs)
{-# INLINE takeN #-}

-- | A single byte.
getWord8 :: Parser Word8
getWord8 = Parser \bs -> case BS.uncons bs of
  Nothing -> Left UnexpectedEndOfInput
  Just (w, rest) -> Right (w, rest)
{-# INLINE getWord8 #-}

-- | A big-endian 16-bit integer.
getWord16be :: Parser Word16
getWord16be = do
  hi <- getWord8
  lo <- getWord8
  pure (fromIntegral hi `shiftL` 8 .|. fromIntegral lo)

-- | A big-endian 32-bit integer.
getWord32be :: Parser Word32
getWord32be = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  d <- getWord8
  pure $
    fromIntegral a `shiftL` 24
      .|. fromIntegral b `shiftL` 16
      .|. fromIntegral c `shiftL` 8
      .|. fromIntegral d

{- | A Variable Byte Integer (§1.5.5): 1–4 bytes, base-128, continuation bit in
the high bit. Rejects encodings longer than 4 bytes and non-minimal encodings.
-}
getVarInt :: Parser Int
getVarInt = go 0 0
  where
    go !acc !count = do
      b <- getWord8
      let acc' = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` (7 * count))
          count' = count + 1
      if b .&. 0x80 /= 0
        then
          if count' >= 4
            then failP MalformedVarInt
            else go acc' count'
        else
          -- minimal-encoding check: a multi-byte encoding whose final byte is 0
          -- (other than the single-byte 0) wasted a continuation byte.
          if count > 0 && b == 0
            then failP MalformedVarInt
            else pure acc'

-- | The number of bytes a value occupies as a Variable Byte Integer.
varIntWidth :: Int -> Int
varIntWidth n
  | n < 0x80 = 1
  | n < 0x4000 = 2
  | n < 0x200000 = 3
  | otherwise = 4

{- | A UTF-8 Encoded String (§1.5.4): a 16-bit length prefix then that many
UTF-8 bytes.
-}
getText :: Parser Text
getText = do
  len <- getWord16be
  raw <- takeN (fromIntegral len)
  case TE.decodeUtf8' raw of
    Left _ -> failP InvalidUtf8
    Right t -> pure t

-- | Binary Data (§1.5.6): a 16-bit length prefix then that many raw bytes.
getBytes :: Parser ByteString
getBytes = do
  len <- getWord16be
  takeN (fromIntegral len)

-- | A UTF-8 String Pair (§1.5.7).
getStringPair :: Parser (Text, Text)
getStringPair = (,) <$> getText <*> getText

-- | All remaining bytes (e.g. a PUBLISH payload).
getRemaining :: Parser ByteString
getRemaining = Parser \bs -> Right (bs, BS.empty)

-- | Is the input exhausted?
atEnd :: Parser Bool
atEnd = Parser \bs -> Right (BS.null bs, bs)

{- | Take @n@ bytes and run a sub-parser that must consume all of them. Used to
isolate a length-delimited region (e.g. the property block) so an overrun or
underrun is reported precisely.
-}
subParse :: Int -> Parser a -> Parser a
subParse n p = do
  region <- takeN n
  either failP pure (runComplete p region)
