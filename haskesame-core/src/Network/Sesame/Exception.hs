module Network.Sesame.Exception (
  SesameException (..),
  SesameProtocolError (..),
  SesameCryptoError (..),
  SesameTransportError (..),
) where

import Control.Exception (Exception)
import Network.Sesame.Types (ItemCode, ResultCode)

data SesameProtocolError
  = EmptyBlePacket
  | EmptyMessage
  | ShortMessage
  | InvalidLength !String !Int !Int
  | InvalidUuid
  | UnexpectedMessage !String
  | OperationFailed !ItemCode !ResultCode
  deriving stock (Show, Eq)

data SesameCryptoError
  = InvalidSecretKeyLength !Int
  | InvalidSessionTokenLength !Int
  | CipherInitFailed !String
  | AuthenticationFailed
  | CiphertextTooShort
  | CounterExhausted
  deriving stock (Show, Eq)

data SesameTransportError
  = TransportClosed
  | TransportCallFailed !String
  | DeviceNotFound !String
  | AdvertisementUnavailable
  deriving stock (Show, Eq)

data SesameException
  = SesameProtocolException !SesameProtocolError
  | SesameCryptoException !SesameCryptoError
  | SesameTransportException !SesameTransportError
  deriving stock (Show, Eq)

instance Exception SesameException
