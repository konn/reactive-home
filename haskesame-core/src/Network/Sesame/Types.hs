module Network.Sesame.Types (
  ProductModel (..),
  ItemCode (..),
  OpCode (..),
  ResultCode (..),
  PacketHeader (..),
  Advertisement (..),
  SesameMessage (..),
  SesameCommand (..),
  SesameResponse (..),
  SesamePublish (..),
  SessionToken (..),
  SecretKey (..),
  SessionKey (..),
  HistoryTag (..),
  Sesame5MechStatus (..),
  Sesame5MechSetting (..),
  batteryVoltage,
  batteryPercentage,
  createHistoryTag,
) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)

data ProductModel
  = Sesame5
  | Sesame5Pro
  | SesameTouchPro
  | SesameTouch
  | Sesame5Usa
  | UnknownProductModel !Word16
  deriving stock (Show, Eq, Ord, Generic)

data ItemCode
  = Registration
  | Login
  | Initial
  | Autolock
  | MechSetting
  | MechStatus
  | Lock
  | Unlock
  | MoveTo
  | Stop
  | Toggle
  | UnknownItemCode !Word8
  deriving stock (Show, Eq, Ord, Generic)

data OpCode = Create | Read | Update | Delete | Sync | Async | Response | Publish | Undefine | UnknownOpCode !Word8
  deriving stock (Show, Eq, Ord, Generic)

data ResultCode = Success | InvalidFormat | NotSupported | StorageFail | InvalidSig | NotFound | Unknown | Busy | InvalidParam | InvalidAction | UnknownResultCode !Word8
  deriving stock (Show, Eq, Ord, Generic)

data PacketHeader = PacketHeader
  { isBeginning :: !Bool
  , isEnd :: !Bool
  , isEncrypted :: !Bool
  }
  deriving stock (Show, Eq, Generic)

data Advertisement = Advertisement
  { productModel :: !ProductModel
  , isRegistered :: !Bool
  , deviceUuid :: !UUID
  }
  deriving stock (Show, Eq, Generic)

data SesameMessage = SesameMessage
  { opCode :: !OpCode
  , payload :: !ByteString
  }
  deriving stock (Show, Eq, Generic)

data SesameCommand = SesameCommand
  { itemCode :: !ItemCode
  , commandPayload :: !ByteString
  }
  deriving stock (Show, Eq, Generic)

data SesameResponse = SesameResponse
  { responseItemCode :: !ItemCode
  , resultCode :: !ResultCode
  , responsePayload :: !ByteString
  }
  deriving stock (Show, Eq, Generic)

data SesamePublish = SesamePublish
  { publishItemCode :: !ItemCode
  , publishPayload :: !ByteString
  }
  deriving stock (Show, Eq, Generic)

newtype SessionToken = SessionToken {unSessionToken :: ByteString}
  deriving stock (Show, Eq, Generic)

newtype SecretKey = SecretKey {unSecretKey :: ByteString}
  deriving stock (Show, Eq, Generic)

newtype SessionKey = SessionKey {unSessionKey :: ByteString}
  deriving stock (Show, Eq, Generic)

newtype HistoryTag = HistoryTag {unHistoryTag :: ByteString}
  deriving stock (Show, Eq, Generic)

data Sesame5MechStatus = Sesame5MechStatus
  { rawBattery :: !Word16
  , target :: !Int16
  , position :: !Int16
  , statusFlags :: !Word8
  }
  deriving stock (Show, Eq, Generic)

data Sesame5MechSetting = Sesame5MechSetting
  { lockPosition :: !Int16
  , unlockPosition :: !Int16
  , autoLockDuration :: !Word16
  }
  deriving stock (Show, Eq, Generic)

batteryVoltage :: Sesame5MechStatus -> Double
batteryVoltage status = fromIntegral status.rawBattery * 2 / 1000

batteryPercentage :: Sesame5MechStatus -> Int
batteryPercentage status = go voltageLevels batteryPercentages
  where
    v = batteryVoltage status
    voltageLevels :: [Double]
    voltageLevels = [5.85, 5.82, 5.79, 5.76, 5.73, 5.70, 5.65, 5.60, 5.55, 5.50, 5.40, 5.20, 5.10, 5.0, 4.8, 4.6]
    batteryPercentages :: [Double]
    batteryPercentages = [100, 95, 90, 85, 80, 70, 60, 50, 40, 32, 21, 13, 10, 7, 3, 0]
    go (upperV : lowerV : restV) (upperP : lowerP : restP)
      | v >= upperV = floor upperP
      | v <= lowerV = go (lowerV : restV) (lowerP : restP)
      | otherwise = floor (((upperP - lowerP) * (v - lowerV) / (upperV - lowerV)) + lowerP)
    go _ (p : _) = floor p
    go _ [] = 0

createHistoryTag :: Text -> HistoryTag
createHistoryTag name =
  let bytes = BS.take 20 (TE.encodeUtf8 name)
   in HistoryTag (BS.cons (fromIntegral (BS.length bytes)) bytes)

_statusFlag :: Word8 -> Sesame5MechStatus -> Bool
_statusFlag flag status = status.statusFlags .&. flag /= 0
