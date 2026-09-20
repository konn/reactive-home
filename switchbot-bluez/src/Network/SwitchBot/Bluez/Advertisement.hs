-- | Pure BlueZ signal accumulation. Cached objects never count as fresh data.
module Network.SwitchBot.Bluez.Advertisement (
  BluezProperties,
  BluezInterfaces,
  ManagedObjects,
  ScanState,
  initialScan,
  collectSignal,
  scanResults,
  scanErrors,
) where

import Control.Applicative ((<|>))
import DBus
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Network.SwitchBot.Advertisement

type BluezProperties = Map String Variant

type BluezInterfaces = Map String BluezProperties

type ManagedObjects = Map ObjectPath BluezInterfaces

data ScanState = ScanState
  { cache :: !(Map ObjectPath BluezProperties)
  , seenManufacturer :: !(Set ObjectPath)
  , readings :: !(Map ObjectPath SensorReading)
  , errors :: !(Map ObjectPath DecodeError)
  }

initialScan :: ObjectPath -> ManagedObjects -> ScanState
initialScan adapter objects =
  ScanState
    (Map.mapMaybe (Map.lookup "org.bluez.Device1") $ Map.filterWithKey (\path _ -> belongsTo adapter path) objects)
    Set.empty
    Map.empty
    Map.empty

scanResults :: ScanState -> [SensorReading]
scanResults = Map.elems . (.readings)

scanErrors :: ScanState -> [DecodeError]
scanErrors = Map.elems . (.errors)

collectSignal :: ObjectPath -> Signal -> ScanState -> ScanState
collectSignal adapter event state
  | event.signalInterface == "org.freedesktop.DBus.Properties"
  , event.signalMember == "PropertiesChanged"
  , [iface, changed, invalidated] <- event.signalBody
  , fromVariant @String iface == Just "org.bluez.Device1"
  , Just props <- fromVariant changed
  , Just removed <- fromVariant invalidated =
      update event.signalPath props removed
  | event.signalInterface == "org.freedesktop.DBus.ObjectManager"
  , event.signalMember == "InterfacesAdded"
  , [rawPath, rawInterfaces] <- event.signalBody
  , Just path <- fromVariant rawPath
  , Just interfaces <- fromVariant @BluezInterfaces rawInterfaces
  , Just props <- Map.lookup "org.bluez.Device1" interfaces =
      update path props []
  | event.signalInterface == "org.freedesktop.DBus.ObjectManager"
  , event.signalMember == "InterfacesRemoved"
  , [rawPath, rawInterfaces] <- event.signalBody
  , Just path <- fromVariant rawPath
  , Just interfaces <- fromVariant @[String] rawInterfaces
  , "org.bluez.Device1" `elem` interfaces =
      state
        { cache = Map.delete path state.cache
        , seenManufacturer = Set.delete path state.seenManufacturer
        , readings = Map.delete path state.readings
        , errors = Map.delete path state.errors
        }
  | otherwise = state
  where
    update path changed invalidated
      | not (belongsTo adapter path) = state
      | otherwise =
          let props = Map.withoutKeys (Map.union changed $ Map.findWithDefault Map.empty path state.cache) $ Set.fromList invalidated
              seen
                | "ManufacturerData" `elem` invalidated = Set.delete path state.seenManufacturer
                | Map.member "ManufacturerData" changed = Set.insert path state.seenManufacturer
                | otherwise = state.seenManufacturer
              next =
                state
                  { cache = Map.insert path props state.cache
                  , seenManufacturer = seen
                  , readings = Map.delete path state.readings
                  , errors = Map.delete path state.errors
                  }
           in if Set.notMember path seen
                then next
                else case decodeProperties props of
                  Right Nothing -> next
                  Right (Just reading) -> next {readings = Map.insert path reading next.readings}
                  Left err -> next {errors = Map.insert path err next.errors}

belongsTo :: ObjectPath -> ObjectPath -> Bool
belongsTo adapter path = (formatObjectPath adapter <> "/") `isPrefixOf` formatObjectPath path

decodeProperties :: BluezProperties -> Either DecodeError (Maybe SensorReading)
decodeProperties props =
  decodeAdvertisement
    Advertisement
      { serviceData = [(T.pack uuid, bytes) | (uuid, value) <- Map.toList services, Just bytes <- [variantBytes value]]
      , manufacturerData = [(company, bytes) | (company, value) <- Map.toList manufacturers, Just bytes <- [variantBytes value]]
      }
  where
    services = fromMaybe Map.empty $ Map.lookup "ServiceData" props >>= fromVariant @(Map String Variant)
    manufacturers = fromMaybe Map.empty $ Map.lookup "ManufacturerData" props >>= fromVariant @(Map Word16 Variant)
    variantBytes value = fromVariant value <|> (BS.pack <$> fromVariant @[Word8] value)
