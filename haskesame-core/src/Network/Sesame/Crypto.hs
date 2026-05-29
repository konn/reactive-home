module Network.Sesame.Crypto (
  SesameCipher,
  newSesameCipher,
  deriveSessionKey,
  encrypt,
  decrypt,
) where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (AEADMode (AEAD_CCM), AuthTag (AuthTag), CCM_L (CCM_L2), CCM_M (CCM_M4), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Cipher.Types qualified as Cipher
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.MAC.CMAC (CMAC, cmac)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word64)
import Network.Sesame.Exception
import Network.Sesame.Types

data SesameCipher = SesameCipher
  { sessionToken :: !SessionToken
  , sessionKey :: !SessionKey
  , encryptCounter :: !(IORef Word64)
  , decryptCounter :: !(IORef Word64)
  }

deriveSessionKey :: SecretKey -> SessionToken -> Either SesameCryptoError SessionKey
deriveSessionKey (SecretKey secret) (SessionToken token)
  | BS.length secret /= 16 = Left (InvalidSecretKeyLength (BS.length secret))
  | BS.length token /= 4 = Left (InvalidSessionTokenLength (BS.length token))
  | otherwise = do
      aes <- initAes secret
      let digest = cmac aes token :: CMAC AES128
      Right (SessionKey (BA.convert digest))

newSesameCipher :: SessionToken -> SessionKey -> IO SesameCipher
newSesameCipher token key = SesameCipher token key <$> newIORef 0 <*> newIORef 0

encrypt :: SesameCipher -> ByteString -> IO (Either SesameCryptoError ByteString)
encrypt cipher plaintext =
  runWithCounter cipher.encryptCounter \counter -> do
    aes <- initAes cipher.sessionKey.unSessionKey
    aead <- initAead aes (nonce cipher.sessionToken counter) (BS.length plaintext)
    let (tag, ciphertext) = aeadSimpleEncrypt aead (BS.singleton 0) plaintext 4
    pure (ciphertext <> BA.convert tag)

decrypt :: SesameCipher -> ByteString -> IO (Either SesameCryptoError ByteString)
decrypt cipher ciphertext
  | BS.length ciphertext < 4 = pure (Left CiphertextTooShort)
  | otherwise =
      runWithCounter cipher.decryptCounter \counter -> do
        aes <- initAes cipher.sessionKey.unSessionKey
        aead <- initAead aes (nonce cipher.sessionToken counter) (BS.length ciphertext - 4)
        maybe
          (Left AuthenticationFailed)
          Right
          (aeadSimpleDecrypt aead (BS.singleton 0) body (AuthTag (BA.convert tag)))
  where
    (body, tag) = BS.splitAt (BS.length ciphertext - 4) ciphertext

runWithCounter :: IORef Word64 -> (Word64 -> Either SesameCryptoError ByteString) -> IO (Either SesameCryptoError ByteString)
runWithCounter ref action = do
  counter <- readIORef ref
  if counter == maxBound
    then pure (Left CounterExhausted)
    else case action counter of
      Left err -> pure (Left err)
      Right bytes -> writeIORef ref (counter + 1) *> pure (Right bytes)

initAes :: ByteString -> Either SesameCryptoError AES128
initAes key = case cipherInit key of
  CryptoPassed aes -> Right aes
  CryptoFailed err -> Left (CipherInitFailed (show err))

initAead :: AES128 -> ByteString -> Int -> Either SesameCryptoError (Cipher.AEAD AES128)
initAead aes n payloadLength = case aeadInit (AEAD_CCM payloadLength CCM_M4 CCM_L2) aes n of
  CryptoPassed aead -> Right aead
  CryptoFailed err -> Left (CipherInitFailed (show err))

nonce :: SessionToken -> Word64 -> ByteString
nonce (SessionToken token) counter =
  BS.pack
    [ fromIntegral counter
    , fromIntegral (counter `div` 0x100)
    , fromIntegral (counter `div` 0x10000)
    , fromIntegral (counter `div` 0x1000000)
    , fromIntegral (counter `div` 0x100000000)
    , fromIntegral (counter `div` 0x10000000000)
    , fromIntegral (counter `div` 0x1000000000000)
    , fromIntegral (counter `div` 0x100000000000000)
    , 0
    ]
    <> token
