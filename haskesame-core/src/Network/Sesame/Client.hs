module Network.Sesame.Client (
  Sesame5Client,
  Sesame5ClientConfig (..),
  defaultSesame5ClientConfig,
  newSesame5Client,
  newSesame5ClientWith,
  login,
  lock,
  unlock,
  toggle,
  setMechSetting,
  setAutoLockDuration,
  readPublish,
) where

import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM (TQueue, TVar, atomically, check, newTQueueIO, newTVarIO, orElse, readTQueue, readTVar, registerDelay, writeTQueue, writeTVar)
import Control.Exception.Safe qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import ListT qualified
import Network.Sesame.Codec
import Network.Sesame.Crypto qualified as Crypto
import Network.Sesame.Exception
import Network.Sesame.Transport
import Network.Sesame.Types
import StmContainers.Map qualified as STMMap

data Sesame5Client = Sesame5Client
  { transport :: !SesameTransport
  , config :: !Sesame5ClientConfig
  , cipherVar :: !(TVar (Maybe Crypto.SesameCipher))
  , responseWaiters :: !(STMMap.Map Word8 (TQueue (Either SesameException SesameResponse)))
  , publishQueue :: !(TQueue (Either SesameException SesamePublish))
  }

data Sesame5ClientConfig = Sesame5ClientConfig
  { commandTimeoutMicros :: !Int
  }
  deriving stock (Show, Eq, Generic)

defaultSesame5ClientConfig :: Sesame5ClientConfig
defaultSesame5ClientConfig =
  Sesame5ClientConfig
    { commandTimeoutMicros = 10000000
    }

newSesame5Client :: SesameTransport -> IO Sesame5Client
newSesame5Client = newSesame5ClientWith defaultSesame5ClientConfig

newSesame5ClientWith :: Sesame5ClientConfig -> SesameTransport -> IO Sesame5Client
newSesame5ClientWith config transport = do
  client <-
    Sesame5Client transport config
      <$> newTVarIO Nothing
      <*> STMMap.newIO
      <*> newTQueueIO
  reader <- async (receiveLoop client)
  link reader
  pure client

login :: Sesame5Client -> SecretKey -> IO Int
login client secret = do
  token <- waitInitial client
  sessionKey <- either (Exception.throwIO . SesameCryptoException) pure (Crypto.deriveSessionKey secret token)
  cipher <- Crypto.newSesameCipher token sessionKey
  atomically (writeTVar client.cipherVar (Just cipher))
  queue <- registerResponseWaiter client Login
  sent <- Exception.tryAny (either (Exception.throwIO . SesameTransportException) pure =<< client.transport.sendBle False (encodeCommand (SesameCommand Login (BS.take 4 sessionKey.unSessionKey))))
  response <- case sent of
    Left err -> unregisterResponseWaiter client Login *> atomically (writeTVar client.cipherVar Nothing) *> Exception.throwIO err
    Right () -> waitRegisteredResponse client Login queue client.config.commandTimeoutMicros
  case response.resultCode of
    Success -> waitLoginPublishes client *> pure (timestampLE response.responsePayload)
    rc -> atomically (writeTVar client.cipherVar Nothing) *> Exception.throwIO (SesameProtocolException (OperationFailed Login rc))

lock :: Sesame5Client -> Text -> IO ()
lock client name = sendEncrypted client (SesameCommand Lock (createHistoryTag name).unHistoryTag)

unlock :: Sesame5Client -> Text -> IO ()
unlock client name = sendEncrypted client (SesameCommand Unlock (createHistoryTag name).unHistoryTag)

toggle :: Sesame5Client -> Text -> IO ()
toggle client name = sendEncrypted client (SesameCommand Toggle (createHistoryTag name).unHistoryTag)

setMechSetting :: Sesame5Client -> Sesame5MechSetting -> IO ()
setMechSetting client setting =
  sendEncrypted client (SesameCommand MechSetting (encodeSesame5MechSetting setting))

setAutoLockDuration :: Sesame5Client -> Word16 -> IO ()
setAutoLockDuration client seconds =
  sendEncrypted client (SesameCommand Autolock (word16LE seconds))

readPublish :: Sesame5Client -> IO SesamePublish
readPublish client = do
  readTQueueOrTimeout client.publishQueue Nothing >>= either Exception.throwIO pure

sendEncrypted :: Sesame5Client -> SesameCommand -> IO ()
sendEncrypted client command = do
  response <- sendEncryptedResponse client command
  case response.resultCode of
    Success -> pure ()
    rc -> Exception.throwIO (SesameProtocolException (OperationFailed command.itemCode rc))

sendEncryptedResponse :: Sesame5Client -> SesameCommand -> IO SesameResponse
sendEncryptedResponse client command = do
  cipher <- atomically (readTVar client.cipherVar) >>= maybe (Exception.throwIO (SesameCryptoException AuthenticationFailed)) pure
  response <- sendCommand client (Just cipher) command
  case response.resultCode of
    Success -> pure response
    rc -> Exception.throwIO (SesameProtocolException (OperationFailed command.itemCode rc))

sendCommand :: Sesame5Client -> Maybe Crypto.SesameCipher -> SesameCommand -> IO SesameResponse
sendCommand client maybeCipher command = do
  queue <- registerResponseWaiter client command.itemCode
  let plaintext = encodeCommand command
  (encrypted, outgoing) <- case maybeCipher of
    Nothing -> pure (False, plaintext)
    Just cipher -> (True,) <$> (Crypto.encrypt cipher plaintext >>= either (Exception.throwIO . SesameCryptoException) pure)
  sent <- Exception.tryAny (either (Exception.throwIO . SesameTransportException) pure =<< client.transport.sendBle encrypted outgoing)
  case sent of
    Left err -> unregisterResponseWaiter client command.itemCode *> Exception.throwIO err
    Right () -> waitRegisteredResponse client command.itemCode queue client.config.commandTimeoutMicros

waitRegisteredResponse :: Sesame5Client -> ItemCode -> TQueue (Either SesameException SesameResponse) -> Int -> IO SesameResponse
waitRegisteredResponse client expected queue timeoutMicros = do
  result <-
    Exception.tryAny (either Exception.throwIO pure =<< readTQueueOrTimeout queue (Just timeoutMicros))
      `Exception.finally` unregisterResponseWaiter client expected
  case result of
    Left err -> Exception.throwIO err
    Right response -> pure response

waitInitial :: Sesame5Client -> IO SessionToken
waitInitial client = do
  publish <- readPublish client
  if publish.publishItemCode == Initial
    then pure (SessionToken publish.publishPayload)
    else waitInitial client

waitLoginPublishes :: Sesame5Client -> IO ()
waitLoginPublishes client = go False False []
  where
    go seenStatus seenSetting seen = do
      if seenStatus && seenSetting
        then atomically (mapM_ (writeTQueue client.publishQueue) (reverse seen))
        else do
          publish <- readTQueueOrTimeout client.publishQueue (Just client.config.commandTimeoutMicros) >>= either Exception.throwIO pure
          let seen' = Right publish : seen
              (seenStatus', seenSetting') = updateSeen seenStatus seenSetting publish
          go seenStatus' seenSetting' seen'

    updateSeen seenStatus seenSetting publish =
      case publish.publishItemCode of
        MechStatus -> (True, seenSetting)
        MechSetting -> (seenStatus, True)
        _ -> (seenStatus, seenSetting)

receiveLoop :: Sesame5Client -> IO ()
receiveLoop client = foreverReceive []
  where
    foreverReceive pendingEncrypted = do
      packet <- client.transport.receiveBle
      case packet of
        Left err -> broadcastException client (SesameTransportException err)
        Right (bytes, encrypted)
          | encrypted -> do
              (pendingEncrypted', result) <- drainEncryptedPending client (pendingEncrypted <> [bytes])
              case result of
                Left err -> broadcastException client err
                Right () -> foreverReceive pendingEncrypted'
          | otherwise -> do
              result <- Exception.try (decodePlaintextIncoming client bytes) :: IO (Either SesameException ())
              case result of
                Left err -> broadcastException client (err :: SesameException)
                Right () -> foreverReceive pendingEncrypted

drainEncryptedPending :: Sesame5Client -> [ByteString] -> IO ([ByteString], Either SesameException ())
drainEncryptedPending client = go [] False
  where
    go deferred progressed [] =
      case (progressed, length deferred > encryptedReorderWindow) of
        (True, _) -> go [] False (reverse deferred)
        (_, True) -> pure (reverse deferred, Left (SesameCryptoException AuthenticationFailed))
        _ -> pure (reverse deferred, Right ())
    go deferred progressed (bytes : rest) = do
      result <- Exception.try do
        plaintext <- decryptEncryptedIncoming client bytes
        decodePlaintextIncoming client plaintext
      case result of
        Right () -> go deferred True rest
        Left (SesameCryptoException AuthenticationFailed) -> go (bytes : deferred) progressed rest
        Left err -> pure (reverse deferred <> rest, Left err)

encryptedReorderWindow :: Int
encryptedReorderWindow = 16

decodePlaintextIncoming :: Sesame5Client -> ByteString -> IO ()
decodePlaintextIncoming client plaintext = do
  message <- either (Exception.throwIO . SesameProtocolException) pure (decodeMessage plaintext)
  case message.opCode of
    Response -> do
      response <- either (Exception.throwIO . SesameProtocolException) pure (decodeResponse message.payload)
      dispatchResponse client response
    Publish -> do
      publish <- either (Exception.throwIO . SesameProtocolException) pure (decodePublish message.payload)
      atomically (writeTQueue client.publishQueue (Right publish))
    other -> Exception.throwIO (SesameProtocolException (UnexpectedMessage ("expected response or publish, got " <> show other)))

decryptEncryptedIncoming :: Sesame5Client -> ByteString -> IO ByteString
decryptEncryptedIncoming client bytes = do
  cipher <- atomically (readTVar client.cipherVar) >>= maybe (Exception.throwIO (SesameCryptoException AuthenticationFailed)) pure
  Crypto.decrypt cipher bytes >>= either (Exception.throwIO . SesameCryptoException) pure

registerResponseWaiter :: Sesame5Client -> ItemCode -> IO (TQueue (Either SesameException SesameResponse))
registerResponseWaiter client item = do
  queue <- newTQueueIO
  atomically (STMMap.insert queue (itemKey item) client.responseWaiters)
  pure queue

unregisterResponseWaiter :: Sesame5Client -> ItemCode -> IO ()
unregisterResponseWaiter client item =
  atomically (STMMap.delete (itemKey item) client.responseWaiters)

dispatchResponse :: Sesame5Client -> SesameResponse -> IO ()
dispatchResponse client response = do
  maybeQueue <- atomically do
    maybeQueue <- STMMap.lookup (itemKey response.responseItemCode) client.responseWaiters
    maybe (pure ()) (`writeTQueue` Right response) maybeQueue
    pure maybeQueue
  case maybeQueue of
    Just _ -> pure ()
    Nothing -> pure ()

broadcastException :: Sesame5Client -> SesameException -> IO ()
broadcastException client err = do
  atomically do
    writeTQueue client.publishQueue (Left err)
    waiters <- ListT.toList (STMMap.listT client.responseWaiters)
    mapM_ ((`writeTQueue` Left err) . snd) waiters

readTQueueOrTimeout :: TQueue a -> Maybe Int -> IO a
readTQueueOrTimeout queue timeoutMicros =
  case timeoutMicros of
    Nothing -> atomically (readTQueue queue)
    Just micros -> do
      timedOut <- registerDelay micros
      atomically
        ( (Right <$> readTQueue queue)
            `orElse` do
              timedOutNow <- readTVar timedOut
              check timedOutNow
              pure (Left (SesameTransportException (TransportCallFailed "Sesame command timed out")))
        )
        >>= \case
          Right value -> pure value
          Left err -> Exception.throwIO err

timestampLE :: ByteString -> Int
timestampLE bytes = sum [fromIntegral (BS.index bytes i) * (256 ^ i) | i <- [0 .. min 3 (BS.length bytes - 1)]]

word16LE :: Word16 -> ByteString
word16LE n = BS.pack [fromIntegral n, fromIntegral (n `div` 256)]

itemKey :: ItemCode -> Word8
itemKey = itemCodeToWord8
