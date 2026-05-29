{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module SimpleBLE (
  Adapter,
  Peripheral,
  Subscription,
  Service (..),
  Characteristic (..),
  ManufacturerData (..),
  SimpleBLEException (..),
  bluetoothEnabled,
  getAdapters,
  adapterIdentifier,
  adapterAddress,
  adapterScanFor,
  adapterScanGetResults,
  peripheralIdentifier,
  peripheralAddress,
  peripheralConnect,
  peripheralDisconnect,
  peripheralIsConnected,
  peripheralServices,
  peripheralManufacturerData,
  peripheralWriteRequest,
  peripheralNotify,
  subscriptionUnsubscribe,
) where

import Control.Exception.Safe (Exception, bracket, throwIO)
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp

C.context Cpp.cppCtx
C.include "<simpleble/Adapter.h>"
C.include "<simpleble/Peripheral.h>"
C.include "<simpleble/Service.h>"
C.include "<simpleble/Characteristic.h>"
C.include "<cstring>"
C.include "<cstdlib>"
C.include "<exception>"
C.include "<map>"
C.include "<string>"
C.include "<vector>"
Cpp.using "namespace SimpleBLE"

C.verbatim
  "typedef void (*simpleble_hs_notify_callback)(const uint8_t*, size_t, void*);\n\
  \static char* simpleble_hs_strdup(const std::string& source) {\n\
  \  char* out = static_cast<char*>(std::malloc(source.size() + 1));\n\
  \  if (out == nullptr) return nullptr;\n\
  \  std::memcpy(out, source.c_str(), source.size() + 1);\n\
  \  return out;\n\
  \}\n\
  \extern \"C\" void simpleble_hs_adapter_delete(void* handle) {\n\
  \  delete static_cast<Adapter*>(handle);\n\
  \}\n\
  \extern \"C\" void simpleble_hs_peripheral_delete(void* handle) {\n\
  \  delete static_cast<Peripheral*>(handle);\n\
  \}\n\
  \extern \"C\" void simpleble_hs_services_delete(void* handle) {\n\
  \  delete static_cast<std::vector<Service>*>(handle);\n\
  \}\n\
  \extern \"C\" void simpleble_hs_characteristics_delete(void* handle) {\n\
  \  delete static_cast<std::vector<Characteristic>*>(handle);\n\
  \}\n\
  \extern \"C\" void simpleble_hs_manufacturer_data_delete(void* handle) {\n\
  \  delete static_cast<std::vector<std::pair<uint16_t, ByteArray>>*>(handle);\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_adapters_count() {\n\
  \  try { return Adapter::get_adapters().size(); } catch (...) { return 0; }\n\
  \}\n\
  \extern \"C\" void* simpleble_hs_adapter_get(size_t index) {\n\
  \  try {\n\
  \    auto adapters = Adapter::get_adapters();\n\
  \    if (index >= adapters.size()) return nullptr;\n\
  \    return new Adapter(adapters[index]);\n\
  \  } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" char* simpleble_hs_adapter_identifier(void* handle) {\n\
  \  try { return simpleble_hs_strdup(static_cast<Adapter*>(handle)->identifier()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" char* simpleble_hs_adapter_address(void* handle) {\n\
  \  try { return simpleble_hs_strdup(static_cast<Adapter*>(handle)->address()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_adapter_scan_for(void* handle, int timeout_ms) {\n\
  \  try { static_cast<Adapter*>(handle)->scan_for(timeout_ms); return 0; } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_adapter_scan_results_count(void* handle) {\n\
  \  try { return static_cast<Adapter*>(handle)->scan_get_results().size(); } catch (...) { return 0; }\n\
  \}\n\
  \extern \"C\" void* simpleble_hs_adapter_scan_result_get(void* handle, size_t index) {\n\
  \  try {\n\
  \    auto peripherals = static_cast<Adapter*>(handle)->scan_get_results();\n\
  \    if (index >= peripherals.size()) return nullptr;\n\
  \    return new Peripheral(peripherals[index]);\n\
  \  } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" char* simpleble_hs_peripheral_identifier(void* handle) {\n\
  \  try { return simpleble_hs_strdup(static_cast<Peripheral*>(handle)->identifier()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" char* simpleble_hs_peripheral_address(void* handle) {\n\
  \  try { return simpleble_hs_strdup(static_cast<Peripheral*>(handle)->address()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_peripheral_connect(void* handle) {\n\
  \  try { static_cast<Peripheral*>(handle)->connect(); return 0; } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_peripheral_disconnect(void* handle) {\n\
  \  try { static_cast<Peripheral*>(handle)->disconnect(); return 0; } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_peripheral_is_connected(void* handle) {\n\
  \  try { return static_cast<Peripheral*>(handle)->is_connected() ? 1 : 0; } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" void* simpleble_hs_peripheral_services(void* handle) {\n\
  \  try { return new std::vector<Service>(static_cast<Peripheral*>(handle)->services()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_services_count(void* handle) {\n\
  \  return static_cast<std::vector<Service>*>(handle)->size();\n\
  \}\n\
  \extern \"C\" char* simpleble_hs_service_uuid(void* handle, size_t index) {\n\
  \  try { return simpleble_hs_strdup(static_cast<std::vector<Service>*>(handle)->at(index).uuid()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_service_data_length(void* handle, size_t index) {\n\
  \  try { return static_cast<std::vector<Service>*>(handle)->at(index).data().size(); } catch (...) { return 0; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_service_data_copy(void* handle, size_t index, uint8_t* out) {\n\
  \  try {\n\
  \    auto data = static_cast<std::vector<Service>*>(handle)->at(index).data();\n\
  \    std::memcpy(out, data.data(), data.size());\n\
  \    return 0;\n\
  \  } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" void* simpleble_hs_service_characteristics(void* handle, size_t index) {\n\
  \  try { return new std::vector<Characteristic>(static_cast<std::vector<Service>*>(handle)->at(index).characteristics()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_characteristics_count(void* handle) {\n\
  \  return static_cast<std::vector<Characteristic>*>(handle)->size();\n\
  \}\n\
  \extern \"C\" char* simpleble_hs_characteristic_uuid(void* handle, size_t index) {\n\
  \  try { return simpleble_hs_strdup(static_cast<std::vector<Characteristic>*>(handle)->at(index).uuid()); } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_characteristic_can_read(void* handle, size_t index) {\n\
  \  try { return static_cast<std::vector<Characteristic>*>(handle)->at(index).can_read() ? 1 : 0; } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_characteristic_can_write_request(void* handle, size_t index) {\n\
  \  try { return static_cast<std::vector<Characteristic>*>(handle)->at(index).can_write_request() ? 1 : 0; } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_characteristic_can_write_command(void* handle, size_t index) {\n\
  \  try { return static_cast<std::vector<Characteristic>*>(handle)->at(index).can_write_command() ? 1 : 0; } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_characteristic_can_notify(void* handle, size_t index) {\n\
  \  try { return static_cast<std::vector<Characteristic>*>(handle)->at(index).can_notify() ? 1 : 0; } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_characteristic_can_indicate(void* handle, size_t index) {\n\
  \  try { return static_cast<std::vector<Characteristic>*>(handle)->at(index).can_indicate() ? 1 : 0; } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" void* simpleble_hs_peripheral_manufacturer_data(void* handle) {\n\
  \  try {\n\
  \    auto data = static_cast<Peripheral*>(handle)->manufacturer_data();\n\
  \    auto out = new std::vector<std::pair<uint16_t, ByteArray>>();\n\
  \    out->reserve(data.size());\n\
  \    for (auto const& entry : data) out->push_back(entry);\n\
  \    return out;\n\
  \  } catch (...) { return nullptr; }\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_manufacturer_data_count(void* handle) {\n\
  \  return static_cast<std::vector<std::pair<uint16_t, ByteArray>>*>(handle)->size();\n\
  \}\n\
  \extern \"C\" int simpleble_hs_manufacturer_data_id(void* handle, size_t index) {\n\
  \  try {\n\
  \    auto data = static_cast<std::vector<std::pair<uint16_t, ByteArray>>*>(handle);\n\
  \    if (index >= data->size()) return -1;\n\
  \    return static_cast<int>(data->at(index).first);\n\
  \  } catch (...) { return -1; }\n\
  \}\n\
  \extern \"C\" size_t simpleble_hs_manufacturer_data_length(void* handle, size_t index) {\n\
  \  try {\n\
  \    auto data = static_cast<std::vector<std::pair<uint16_t, ByteArray>>*>(handle);\n\
  \    if (index >= data->size()) return 0;\n\
  \    return data->at(index).second.size();\n\
  \  } catch (...) { return 0; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_manufacturer_data_copy(void* handle, size_t index, uint8_t* out) {\n\
  \  try {\n\
  \    auto data = static_cast<std::vector<std::pair<uint16_t, ByteArray>>*>(handle);\n\
  \    if (index >= data->size()) return 1;\n\
  \    auto payload = data->at(index).second;\n\
  \    std::memcpy(out, payload.data(), payload.size());\n\
  \    return 0;\n\
  \  } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_peripheral_write_request(void* handle, const char* service, const char* characteristic, const uint8_t* data, size_t data_length) {\n\
  \  try { static_cast<Peripheral*>(handle)->write_request(service, characteristic, ByteArray(data, data_length)); return 0; } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_peripheral_notify(void* handle, const char* service, const char* characteristic, void* callback_raw, void* userdata) {\n\
  \  try {\n\
  \    auto callback = reinterpret_cast<simpleble_hs_notify_callback>(callback_raw);\n\
  \    static_cast<Peripheral*>(handle)->notify(service, characteristic, [callback, userdata](ByteArray payload) {\n\
  \      callback(payload.data(), payload.size(), userdata);\n\
  \    });\n\
  \    return 0;\n\
  \  } catch (...) { return 1; }\n\
  \}\n\
  \extern \"C\" int simpleble_hs_peripheral_unsubscribe(void* handle, const char* service, const char* characteristic) {\n\
  \  try { static_cast<Peripheral*>(handle)->unsubscribe(service, characteristic); return 0; } catch (...) { return 1; }\n\
  \}\n"

newtype Adapter = Adapter (ForeignPtr ())

newtype Peripheral = Peripheral (ForeignPtr ())

data Subscription = Subscription
  { peripheral :: !Peripheral
  , service :: !Text
  , characteristic :: !Text
  , callback :: !(FunPtr NotifyCallback)
  , stablePtr :: !(StablePtr (ByteString -> IO ()))
  }

data Service = Service
  { uuid :: !Text
  , data_ :: !ByteString
  , characteristics :: ![Characteristic]
  }
  deriving stock (Show, Eq)

data Characteristic = Characteristic
  { uuid :: !Text
  , canRead :: !Bool
  , canWriteRequest :: !Bool
  , canWriteCommand :: !Bool
  , canNotify :: !Bool
  , canIndicate :: !Bool
  }
  deriving stock (Show, Eq)

data ManufacturerData = ManufacturerData
  { manufacturerId :: !Word16
  , payload :: !ByteString
  }
  deriving stock (Show, Eq)

data SimpleBLEException = SimpleBLEException !String
  deriving stock (Show, Eq)

instance Exception SimpleBLEException

type NotifyCallback = Ptr Word8 -> CSize -> Ptr () -> IO ()

foreign import ccall unsafe "&simpleble_hs_adapter_delete"
  adapterDeleteFinalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "&simpleble_hs_peripheral_delete"
  peripheralDeleteFinalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "&simpleble_hs_services_delete"
  servicesDeleteFinalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "&simpleble_hs_characteristics_delete"
  characteristicsDeleteFinalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "&simpleble_hs_manufacturer_data_delete"
  manufacturerDataDeleteFinalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall "wrapper"
  mkNotifyCallback :: NotifyCallback -> IO (FunPtr NotifyCallback)

bluetoothEnabled :: IO Bool
bluetoothEnabled =
  toBool <$> [C.exp| bool { Adapter::bluetooth_enabled() } |]

getAdapters :: IO [Adapter]
getAdapters = do
  count <- [C.exp| size_t { simpleble_hs_adapters_count() } |]
  forM [0 .. count - 1] \index -> do
    ptr <- [C.exp| void* { simpleble_hs_adapter_get($(size_t index)) } |]
    newAdapter ptr

adapterIdentifier :: Adapter -> IO Text
adapterIdentifier adapter = withAdapter adapter \ptr ->
  takeCString =<< [C.exp| char* { simpleble_hs_adapter_identifier($(void* ptr)) } |]

adapterAddress :: Adapter -> IO Text
adapterAddress adapter = withAdapter adapter \ptr ->
  takeCString =<< [C.exp| char* { simpleble_hs_adapter_address($(void* ptr)) } |]

adapterScanFor :: Adapter -> Int -> IO ()
adapterScanFor adapter timeoutMs =
  withAdapter adapter \ptr ->
    checkError "adapterScanFor" =<< [C.exp| int { simpleble_hs_adapter_scan_for($(void* ptr), $(int timeoutMs')) } |]
  where
    timeoutMs' = fromIntegral timeoutMs :: CInt

adapterScanGetResults :: Adapter -> IO [Peripheral]
adapterScanGetResults adapter =
  withAdapter adapter \ptr -> do
    count <- [C.exp| size_t { simpleble_hs_adapter_scan_results_count($(void* ptr)) } |]
    forM [0 .. count - 1] \index -> do
      peripheral <- [C.exp| void* { simpleble_hs_adapter_scan_result_get($(void* ptr), $(size_t index)) } |]
      newPeripheral peripheral

peripheralIdentifier :: Peripheral -> IO Text
peripheralIdentifier peripheral = withPeripheral peripheral \ptr ->
  takeCString =<< [C.exp| char* { simpleble_hs_peripheral_identifier($(void* ptr)) } |]

peripheralAddress :: Peripheral -> IO Text
peripheralAddress peripheral = withPeripheral peripheral \ptr ->
  takeCString =<< [C.exp| char* { simpleble_hs_peripheral_address($(void* ptr)) } |]

peripheralConnect :: Peripheral -> IO ()
peripheralConnect peripheral =
  withPeripheral peripheral \ptr ->
    checkError "peripheralConnect" =<< [C.exp| int { simpleble_hs_peripheral_connect($(void* ptr)) } |]

peripheralDisconnect :: Peripheral -> IO ()
peripheralDisconnect peripheral =
  withPeripheral peripheral \ptr ->
    checkError "peripheralDisconnect" =<< [C.exp| int { simpleble_hs_peripheral_disconnect($(void* ptr)) } |]

peripheralIsConnected :: Peripheral -> IO Bool
peripheralIsConnected peripheral =
  withPeripheral peripheral \ptr -> do
    result <- [C.exp| int { simpleble_hs_peripheral_is_connected($(void* ptr)) } |]
    if result < 0
      then throwIO (SimpleBLEException "peripheralIsConnected failed")
      else pure (result /= 0)

peripheralServices :: Peripheral -> IO [Service]
peripheralServices peripheral =
  withPeripheral peripheral \ptr ->
    bracket
      ([C.exp| void* { simpleble_hs_peripheral_services($(void* ptr)) } |] >>= newServicesPtr)
      finalizeForeignPtr
      \servicesFp ->
        withForeignPtr servicesFp \servicesPtr -> do
          count <- [C.exp| size_t { simpleble_hs_services_count($(void* servicesPtr)) } |]
          forM [0 .. count - 1] (readService servicesPtr)

peripheralManufacturerData :: Peripheral -> IO [ManufacturerData]
peripheralManufacturerData peripheral =
  withPeripheral peripheral \ptr ->
    bracket
      ([C.exp| void* { simpleble_hs_peripheral_manufacturer_data($(void* ptr)) } |] >>= newManufacturerDataPtr)
      finalizeForeignPtr
      \manufacturerDataFp ->
        withForeignPtr manufacturerDataFp \manufacturerDataPtr -> do
          count <- [C.exp| size_t { simpleble_hs_manufacturer_data_count($(void* manufacturerDataPtr)) } |]
          forM [0 .. count - 1] (readManufacturerData manufacturerDataPtr)

peripheralWriteRequest :: Peripheral -> Text -> Text -> ByteString -> IO ()
peripheralWriteRequest peripheral service characteristic payload =
  withPeripheral peripheral \ptr ->
    withTextCString service \servicePtr ->
      withTextCString characteristic \characteristicPtr ->
        BS.unsafeUseAsCStringLen payload \(payloadPtr, payloadLength) ->
          let payloadLength' = fromIntegral payloadLength :: CSize
           in checkError "peripheralWriteRequest"
                =<< [C.exp| int { simpleble_hs_peripheral_write_request($(void* ptr), $(char* servicePtr), $(char* characteristicPtr), (const uint8_t*)$(char* payloadPtr), $(size_t payloadLength')) } |]

peripheralNotify :: Peripheral -> Text -> Text -> (ByteString -> IO ()) -> IO Subscription
peripheralNotify peripheral service characteristic callback = do
  stable <- newStablePtr callback
  function <- mkNotifyCallback notifyTrampoline
  let userdata = castStablePtrToPtr stable
      callbackPtr = castFunPtrToPtr function
  result <-
    withPeripheral peripheral \ptr ->
      withTextCString service \servicePtr ->
        withTextCString characteristic \characteristicPtr ->
          [C.exp| int { simpleble_hs_peripheral_notify($(void* ptr), $(char* servicePtr), $(char* characteristicPtr), $(void* callbackPtr), $(void* userdata)) } |]
  if result == 0
    then pure (Subscription peripheral service characteristic function stable)
    else do
      freeHaskellFunPtr function
      freeStablePtr stable
      throwIO (SimpleBLEException "peripheralNotify failed")

subscriptionUnsubscribe :: Subscription -> IO ()
subscriptionUnsubscribe Subscription {..} = do
  withPeripheral peripheral \ptr ->
    withTextCString service \servicePtr ->
      withTextCString characteristic \characteristicPtr ->
        checkError "subscriptionUnsubscribe"
          =<< [C.exp| int { simpleble_hs_peripheral_unsubscribe($(void* ptr), $(char* servicePtr), $(char* characteristicPtr)) } |]
  freeHaskellFunPtr callback
  freeStablePtr stablePtr

readService :: Ptr () -> CSize -> IO Service
readService servicesPtr index = do
  uuid <- takeCString =<< [C.exp| char* { simpleble_hs_service_uuid($(void* servicesPtr), $(size_t index)) } |]
  payloadLength <- [C.exp| size_t { simpleble_hs_service_data_length($(void* servicesPtr), $(size_t index)) } |]
  data_ <-
    copyByteString payloadLength \out ->
      [C.exp| int { simpleble_hs_service_data_copy($(void* servicesPtr), $(size_t index), $(uint8_t* out)) } |]
  characteristics <-
    bracket
      ([C.exp| void* { simpleble_hs_service_characteristics($(void* servicesPtr), $(size_t index)) } |] >>= newCharacteristicsPtr)
      finalizeForeignPtr
      \characteristicsFp ->
        withForeignPtr characteristicsFp \characteristicsPtr -> do
          count <- [C.exp| size_t { simpleble_hs_characteristics_count($(void* characteristicsPtr)) } |]
          forM [0 .. count - 1] (readCharacteristic characteristicsPtr)
  pure Service {..}

readCharacteristic :: Ptr () -> CSize -> IO Characteristic
readCharacteristic characteristicsPtr index = do
  uuid <- takeCString =<< [C.exp| char* { simpleble_hs_characteristic_uuid($(void* characteristicsPtr), $(size_t index)) } |]
  canRead <- readBool "canRead" [C.exp| int { simpleble_hs_characteristic_can_read($(void* characteristicsPtr), $(size_t index)) } |]
  canWriteRequest <- readBool "canWriteRequest" [C.exp| int { simpleble_hs_characteristic_can_write_request($(void* characteristicsPtr), $(size_t index)) } |]
  canWriteCommand <- readBool "canWriteCommand" [C.exp| int { simpleble_hs_characteristic_can_write_command($(void* characteristicsPtr), $(size_t index)) } |]
  canNotify <- readBool "canNotify" [C.exp| int { simpleble_hs_characteristic_can_notify($(void* characteristicsPtr), $(size_t index)) } |]
  canIndicate <- readBool "canIndicate" [C.exp| int { simpleble_hs_characteristic_can_indicate($(void* characteristicsPtr), $(size_t index)) } |]
  pure Characteristic {..}

readManufacturerData :: Ptr () -> CSize -> IO ManufacturerData
readManufacturerData manufacturerDataPtr index = do
  rawId <- [C.exp| int { simpleble_hs_manufacturer_data_id($(void* manufacturerDataPtr), $(size_t index)) } |]
  if rawId < 0
    then throwIO (SimpleBLEException "peripheralManufacturerData failed")
    else do
      payloadLength <- [C.exp| size_t { simpleble_hs_manufacturer_data_length($(void* manufacturerDataPtr), $(size_t index)) } |]
      payload <-
        copyByteString payloadLength \out ->
          [C.exp| int { simpleble_hs_manufacturer_data_copy($(void* manufacturerDataPtr), $(size_t index), $(uint8_t* out)) } |]
      pure (ManufacturerData (fromIntegral rawId) payload)

notifyTrampoline :: NotifyCallback
notifyTrampoline bytes length_ userdata = do
  callback <- deRefStablePtr (castPtrToStablePtr userdata)
  callback =<< BS.packCStringLen (castPtr bytes, fromIntegral length_)

newAdapter :: Ptr () -> IO Adapter
newAdapter ptr
  | ptr == nullPtr = throwIO (SimpleBLEException "failed to acquire SimpleBLE adapter")
  | otherwise = Adapter <$> newForeignPtr adapterDeleteFinalizer ptr

newPeripheral :: Ptr () -> IO Peripheral
newPeripheral ptr
  | ptr == nullPtr = throwIO (SimpleBLEException "failed to acquire SimpleBLE peripheral")
  | otherwise = Peripheral <$> newForeignPtr peripheralDeleteFinalizer ptr

newServicesPtr :: Ptr () -> IO (ForeignPtr ())
newServicesPtr ptr
  | ptr == nullPtr = throwIO (SimpleBLEException "failed to read SimpleBLE services")
  | otherwise = newForeignPtr servicesDeleteFinalizer ptr

newCharacteristicsPtr :: Ptr () -> IO (ForeignPtr ())
newCharacteristicsPtr ptr
  | ptr == nullPtr = throwIO (SimpleBLEException "failed to read SimpleBLE characteristics")
  | otherwise = newForeignPtr characteristicsDeleteFinalizer ptr

newManufacturerDataPtr :: Ptr () -> IO (ForeignPtr ())
newManufacturerDataPtr ptr
  | ptr == nullPtr = throwIO (SimpleBLEException "failed to read SimpleBLE manufacturer data")
  | otherwise = newForeignPtr manufacturerDataDeleteFinalizer ptr

withAdapter :: Adapter -> (Ptr () -> IO a) -> IO a
withAdapter (Adapter fp) = withForeignPtr fp

withPeripheral :: Peripheral -> (Ptr () -> IO a) -> IO a
withPeripheral (Peripheral fp) = withForeignPtr fp

withTextCString :: Text -> (CString -> IO a) -> IO a
withTextCString = BS.useAsCString . TE.encodeUtf8

takeCString :: CString -> IO Text
takeCString ptr
  | ptr == nullPtr = throwIO (SimpleBLEException "failed to read SimpleBLE string")
  | otherwise = do
      bytes <- BS.packCString ptr
      [C.exp| void { std::free($(char* ptr)) } |]
      pure (TE.decodeUtf8 bytes)

copyByteString :: CSize -> (Ptr Word8 -> IO CInt) -> IO ByteString
copyByteString size action
  | size == 0 = pure BS.empty
  | otherwise =
      allocaBytes (fromIntegral size) \out -> do
        checkError "copyByteString" =<< action out
        BS.packCStringLen (castPtr out, fromIntegral size)

readBool :: String -> IO CInt -> IO Bool
readBool label action = do
  result <- action
  if result < 0
    then throwIO (SimpleBLEException (label <> " failed"))
    else pure (result /= 0)

checkError :: String -> CInt -> IO ()
checkError label result
  | result == 0 = pure ()
  | otherwise = throwIO (SimpleBLEException (label <> " failed"))
