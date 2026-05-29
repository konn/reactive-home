module Network.Sesame.Client (
  Sesame5Client,
  newSesame5Client,
  login,
  lock,
  unlock,
  toggle,
  setLockPosition,
  setAutoLockDuration,
  readPublish,
) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Text (Text)
import Data.Word (Word16)
import Network.Sesame.Codec
import Network.Sesame.Crypto qualified as Crypto
import Network.Sesame.Exception
import Network.Sesame.Transport
import Network.Sesame.Types

data Sesame5Client = Sesame5Client
  { transport :: !SesameTransport
  , cipherVar :: !(TVar (Maybe Crypto.SesameCipher))
  }

newSesame5Client :: SesameTransport -> IO Sesame5Client
newSesame5Client transport = Sesame5Client transport <$> newTVarIO Nothing

login :: Sesame5Client -> SecretKey -> IO Int
login client secret = do
  token <- waitInitial client
  sessionKey <- either (throwIO . SesameCryptoException) pure (Crypto.deriveSessionKey secret token)
  cipher <- Crypto.newSesameCipher token sessionKey
  response <- sendCommand client Nothing (SesameCommand Login (BS.take 4 sessionKey.unSessionKey))
  case response.resultCode of
    Success -> atomically (writeTVar client.cipherVar (Just cipher)) *> pure (timestampLE response.responsePayload)
    rc -> throwIO (SesameProtocolException (OperationFailed Login rc))

lock :: Sesame5Client -> Text -> IO ()
lock client name = sendEncrypted client (SesameCommand Lock (createHistoryTag name).unHistoryTag)

unlock :: Sesame5Client -> Text -> IO ()
unlock client name = sendEncrypted client (SesameCommand Unlock (createHistoryTag name).unHistoryTag)

toggle :: Sesame5Client -> Text -> IO ()
toggle client name = sendEncrypted client (SesameCommand Toggle (createHistoryTag name).unHistoryTag)

setLockPosition :: Sesame5Client -> Int16 -> Int16 -> IO ()
setLockPosition client lockPosition unlockPosition =
  sendEncrypted client (SesameCommand MechSetting (int16LE lockPosition <> int16LE unlockPosition))

setAutoLockDuration :: Sesame5Client -> Word16 -> IO ()
setAutoLockDuration client seconds =
  sendEncrypted client (SesameCommand Autolock (word16LE seconds))

readPublish :: Sesame5Client -> IO SesamePublish
readPublish client = do
  (bytes, encrypted) <- receiveOrThrow client
  payload <-
    if encrypted
      then decryptWithCurrentCipher client bytes
      else pure bytes
  message <- either (throwIO . SesameProtocolException) pure (decodeMessage payload)
  case message.opCode of
    Publish -> either (throwIO . SesameProtocolException) pure (decodePublish message.payload)
    other -> throwIO (SesameProtocolException (UnexpectedMessage ("expected publish, got " <> show other)))

sendEncrypted :: Sesame5Client -> SesameCommand -> IO ()
sendEncrypted client command = do
  cipher <- atomically (readTVar client.cipherVar) >>= maybe (throwIO (SesameCryptoException AuthenticationFailed)) pure
  response <- sendCommand client (Just cipher) command
  case response.resultCode of
    Success -> pure ()
    rc -> throwIO (SesameProtocolException (OperationFailed command.itemCode rc))

sendCommand :: Sesame5Client -> Maybe Crypto.SesameCipher -> SesameCommand -> IO SesameResponse
sendCommand client maybeCipher command = do
  let plaintext = encodeCommand command
  (encrypted, outgoing) <- case maybeCipher of
    Nothing -> pure (False, plaintext)
    Just cipher -> (True,) <$> (Crypto.encrypt cipher plaintext >>= either (throwIO . SesameCryptoException) pure)
  either (throwIO . SesameTransportException) pure =<< client.transport.sendBle encrypted outgoing
  waitResponse client maybeCipher command.itemCode

waitResponse :: Sesame5Client -> Maybe Crypto.SesameCipher -> ItemCode -> IO SesameResponse
waitResponse client maybeCipher expected = do
  (bytes, encrypted) <- receiveOrThrow client
  payload <- case (encrypted, maybeCipher) of
    (False, _) -> pure bytes
    (True, Just cipher) -> Crypto.decrypt cipher bytes >>= either (throwIO . SesameCryptoException) pure
    (True, Nothing) -> throwIO (SesameCryptoException AuthenticationFailed)
  message <- either (throwIO . SesameProtocolException) pure (decodeMessage payload)
  case message.opCode of
    Response -> do
      response <- either (throwIO . SesameProtocolException) pure (decodeResponse message.payload)
      if response.responseItemCode == expected
        then pure response
        else waitResponse client maybeCipher expected
    Publish -> waitResponse client maybeCipher expected
    other -> throwIO (SesameProtocolException (UnexpectedMessage ("expected response, got " <> show other)))

waitInitial :: Sesame5Client -> IO SessionToken
waitInitial client = do
  (bytes, encrypted) <- receiveOrThrow client
  if encrypted
    then waitInitial client
    else do
      message <- either (throwIO . SesameProtocolException) pure (decodeMessage bytes)
      case message.opCode of
        Publish -> do
          publish <- either (throwIO . SesameProtocolException) pure (decodePublish message.payload)
          if publish.publishItemCode == Initial
            then pure (SessionToken publish.publishPayload)
            else waitInitial client
        _ -> waitInitial client

receiveOrThrow :: Sesame5Client -> IO (ByteString, Bool)
receiveOrThrow client =
  client.transport.receiveBle >>= either (throwIO . SesameTransportException) pure

decryptWithCurrentCipher :: Sesame5Client -> ByteString -> IO ByteString
decryptWithCurrentCipher client bytes = do
  cipher <- atomically (readTVar client.cipherVar) >>= maybe (throwIO (SesameCryptoException AuthenticationFailed)) pure
  Crypto.decrypt cipher bytes >>= either (throwIO . SesameCryptoException) pure

timestampLE :: ByteString -> Int
timestampLE bytes = sum [fromIntegral (BS.index bytes i) * (256 ^ i) | i <- [0 .. min 3 (BS.length bytes - 1)]]

word16LE :: Word16 -> ByteString
word16LE n = BS.pack [fromIntegral n, fromIntegral (n `div` 256)]

int16LE :: Int16 -> ByteString
int16LE = word16LE . fromIntegral @Int16 @Word16
