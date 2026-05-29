module Network.Sesame.Codec (
  encodeHeader,
  decodeHeader,
  fragment,
  Reassembly (..),
  emptyReassembly,
  pushFragment,
  encodeCommand,
  encodeMessage,
  decodeMessage,
  decodeResponse,
  decodePublish,
  decodeAdvertisement,
  decodeSesame5MechStatus,
  decodeSesame5MechSetting,
  itemCodeToWord8,
  opCodeToWord8,
  resultCodeToWord8,
) where

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16)
import Data.UUID qualified as UUID
import Data.Word (Word16, Word8)
import Network.Sesame.Exception
import Network.Sesame.Types

mtuSize :: Int
mtuSize = 20

encodeHeader :: PacketHeader -> Word8
encodeHeader header =
  beginning .|. ending
  where
    beginning = if header.isBeginning then 0x01 else 0
    ending
      | not header.isEnd = 0
      | header.isEncrypted = 0x04
      | otherwise = 0x02

decodeHeader :: Word8 -> PacketHeader
decodeHeader header =
  PacketHeader
    { isBeginning = header .&. 0x01 /= 0
    , isEnd = header .&. 0x06 /= 0
    , isEncrypted = header .&. 0x04 /= 0
    }

fragment :: Bool -> ByteString -> [ByteString]
fragment encrypted bytes
  | BS.null bytes = [BS.singleton (encodeHeader (PacketHeader True True encrypted))]
  | otherwise = go 0
  where
    payloadMax = mtuSize - 1
    total = BS.length bytes
    go offset
      | offset >= total = []
      | otherwise =
          let chunk = BS.take payloadMax (BS.drop offset bytes)
              header = PacketHeader (offset == 0) (offset + payloadMax >= total) encrypted
           in BS.cons (encodeHeader header) chunk : go (offset + payloadMax)

data Reassembly = Reassembly
  { reassemblyBuffer :: !ByteString
  }
  deriving stock (Show, Eq)

emptyReassembly :: Reassembly
emptyReassembly = Reassembly BS.empty

pushFragment :: Reassembly -> ByteString -> Either SesameProtocolError (Reassembly, Maybe (ByteString, Bool))
pushFragment _ bytes | BS.null bytes = Left EmptyBlePacket
pushFragment state bytes =
  let header = decodeHeader (BS.head bytes)
      payload = BS.tail bytes
      buffer = (if header.isBeginning then BS.empty else state.reassemblyBuffer) <> payload
      next = Reassembly buffer
   in if header.isEnd
        then Right (emptyReassembly, Just (buffer, header.isEncrypted))
        else Right (next, Nothing)

encodeCommand :: SesameCommand -> ByteString
encodeCommand command = BS.cons (itemCodeToWord8 command.itemCode) command.commandPayload

encodeMessage :: OpCode -> ByteString -> ByteString
encodeMessage op payload = BS.cons (opCodeToWord8 op) payload

decodeMessage :: ByteString -> Either SesameProtocolError SesameMessage
decodeMessage bytes
  | BS.null bytes = Left EmptyMessage
  | otherwise = Right (SesameMessage (word8ToOpCode (BS.head bytes)) (BS.tail bytes))

decodeResponse :: ByteString -> Either SesameProtocolError SesameResponse
decodeResponse bytes
  | BS.length bytes < 2 = Left ShortMessage
  | otherwise = Right (SesameResponse (word8ToItemCode (BS.index bytes 0)) (word8ToResultCode (BS.index bytes 1)) (BS.drop 2 bytes))

decodePublish :: ByteString -> Either SesameProtocolError SesamePublish
decodePublish bytes
  | BS.null bytes = Left ShortMessage
  | otherwise = Right (SesamePublish (word8ToItemCode (BS.head bytes)) (BS.tail bytes))

decodeAdvertisement :: ByteString -> Either SesameProtocolError Advertisement
decodeAdvertisement bytes
  | BS.length bytes /= 19 = Left (InvalidLength "advertisement" 19 (BS.length bytes))
  | otherwise =
      maybe
        (Left InvalidUuid)
        (\uuid -> Right (Advertisement (word16ToProductModel (le16 0)) (BS.index bytes 2 /= 0) uuid))
        (UUID.fromByteString (LBS.fromStrict (BS.drop 3 bytes)))
  where
    le16 i = fromIntegral (BS.index bytes i) + fromIntegral (BS.index bytes (i + 1)) * 256

decodeSesame5MechStatus :: ByteString -> Either SesameProtocolError Sesame5MechStatus
decodeSesame5MechStatus bytes
  | BS.length bytes /= 7 = Left (InvalidLength "mech status" 7 (BS.length bytes))
  | otherwise = Right (Sesame5MechStatus (le16 0) (sle16 2) (sle16 4) (BS.index bytes 6))
  where
    le16 = readWord16LE bytes
    sle16 = fromIntegral @Word16 @Int16 . readWord16LE bytes

decodeSesame5MechSetting :: ByteString -> Either SesameProtocolError Sesame5MechSetting
decodeSesame5MechSetting bytes
  | BS.length bytes /= 6 = Left (InvalidLength "mech setting" 6 (BS.length bytes))
  | otherwise = Right (Sesame5MechSetting (sle16 0) (sle16 2) (le16 4))
  where
    le16 = readWord16LE bytes
    sle16 = fromIntegral @Word16 @Int16 . readWord16LE bytes

readWord16LE :: ByteString -> Int -> Word16
readWord16LE bytes i = fromIntegral (BS.index bytes i) + fromIntegral (BS.index bytes (i + 1)) * 256

itemCodeToWord8 :: ItemCode -> Word8
itemCodeToWord8 = \case
  Registration -> 1
  Login -> 2
  Initial -> 14
  Autolock -> 11
  MechSetting -> 80
  MechStatus -> 81
  Lock -> 82
  Unlock -> 83
  MoveTo -> 84
  Stop -> 86
  Toggle -> 88
  UnknownItemCode x -> x

word8ToItemCode :: Word8 -> ItemCode
word8ToItemCode = \case
  1 -> Registration
  2 -> Login
  11 -> Autolock
  14 -> Initial
  80 -> MechSetting
  81 -> MechStatus
  82 -> Lock
  83 -> Unlock
  84 -> MoveTo
  86 -> Stop
  88 -> Toggle
  x -> UnknownItemCode x

opCodeToWord8 :: OpCode -> Word8
opCodeToWord8 = \case
  Create -> 0x01
  Read -> 0x02
  Update -> 0x03
  Delete -> 0x04
  Sync -> 0x05
  Async -> 0x06
  Response -> 0x07
  Publish -> 0x08
  Undefine -> 0x10
  UnknownOpCode x -> x

word8ToOpCode :: Word8 -> OpCode
word8ToOpCode = \case
  0x01 -> Create
  0x02 -> Read
  0x03 -> Update
  0x04 -> Delete
  0x05 -> Sync
  0x06 -> Async
  0x07 -> Response
  0x08 -> Publish
  0x10 -> Undefine
  x -> UnknownOpCode x

resultCodeToWord8 :: ResultCode -> Word8
resultCodeToWord8 = \case
  Success -> 0
  InvalidFormat -> 1
  NotSupported -> 2
  StorageFail -> 3
  InvalidSig -> 4
  NotFound -> 5
  Unknown -> 6
  Busy -> 7
  InvalidParam -> 8
  InvalidAction -> 9
  UnknownResultCode x -> x

word8ToResultCode :: Word8 -> ResultCode
word8ToResultCode = \case
  0 -> Success
  1 -> InvalidFormat
  2 -> NotSupported
  3 -> StorageFail
  4 -> InvalidSig
  5 -> NotFound
  6 -> Unknown
  7 -> Busy
  8 -> InvalidParam
  9 -> InvalidAction
  x -> UnknownResultCode x

word16ToProductModel :: Word16 -> ProductModel
word16ToProductModel = \case
  5 -> Sesame5
  7 -> Sesame5Pro
  9 -> SesameTouchPro
  10 -> SesameTouch
  16 -> Sesame5Usa
  x -> UnknownProductModel x
