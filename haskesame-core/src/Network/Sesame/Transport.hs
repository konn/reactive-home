module Network.Sesame.Transport (
  SesameTransport (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Network.Sesame.Exception
import Network.Sesame.Types

data SesameTransport = SesameTransport
  { sendBle :: Bool -> ByteString -> IO (Either SesameTransportError ())
  , receiveBle :: IO (Either SesameTransportError (ByteString, Bool))
  , closeBle :: IO ()
  , advertisement :: IO (Either SesameTransportError Advertisement)
  }
  deriving stock (Generic)
