{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The local Hometrics REST API. Cloud credentials belong to Hometrics.
module Home.Reactive.Metrics.Hometrics (
  HometricsConfig (..),
  HometricsSensorConfig (..),
  HometricsField (..),
  defaultHometricsSensorConfig,
  hometricsFields,
  hometricsSensorName,
  hometricsSensorCodec,
  hometricsPayload,
  postHometrics,
) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Aeson qualified as A
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Home.Reactive.Sensor (SensorSample (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.SwitchBot.Advertisement (SensorReading (..))
import Toml (HasCodec, TomlTable (..), (.=))
import Toml qualified

newtype HometricsConfig = HometricsConfig {endpoint :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (HasCodec) via TomlTable HometricsConfig

data HometricsField = Temperature | Humidity | CO2
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data HometricsSensorConfig = HometricsSensorConfig
  { name :: !(Maybe Text)
  , fields :: !(Maybe [HometricsField])
  }
  deriving stock (Show, Eq, Ord, Generic)

hometricsSensorCodec :: Toml.TomlCodec HometricsSensorConfig
hometricsSensorCodec =
  HometricsSensorConfig
    <$> Toml.dioptional (Toml.text "hometrics_name") .= (.name)
    <*> Toml.dioptional (Toml.arrayOf (Toml._TextBy fieldName parseField) "hometrics_fields") .= (.fields)
  where
    fieldName Temperature = "temperature"
    fieldName Humidity = "humidity"
    fieldName CO2 = "co2"
    parseField "temperature" = Right Temperature
    parseField "humidity" = Right Humidity
    parseField "co2" = Right CO2
    parseField other = Left $ "unsupported Hometrics field: " <> other <> "; expected temperature, humidity or co2"

defaultHometricsSensorConfig :: HometricsSensorConfig
defaultHometricsSensorConfig = HometricsSensorConfig Nothing Nothing

hometricsFields :: HometricsSensorConfig -> [HometricsField]
hometricsFields = Set.toList . Set.fromList . fromMaybe [minBound .. maxBound] . (.fields)

hometricsSensorName :: Text -> HometricsSensorConfig -> Text
hometricsSensorName sensor = fromMaybe sensor . (.name)

{- | Empty maps are omitted. Never send a request with no usable measurements.
CO2 requires the Hometrics REST API extension; older servers ignore unknown
JSON fields, so deploy the corresponding Hometrics update before using it.
-}
hometricsPayload :: Map Text HometricsSensorConfig -> [SensorSample] -> Maybe A.Value
hometricsPayload sensors samples =
  let measurements :: HometricsField -> (SensorReading -> Maybe a) -> Map Text a
      measurements field extract =
        Map.fromList
          [ (hometricsSensorName s.sensor cfg, v)
          | s <- samples
          , Just cfg <- [Map.lookup s.sensor sensors]
          , field `elem` hometricsFields cfg
          , Just v <- [extract s.reading]
          ]
      temperatures = measurements Temperature (.temperatureC)
      humidity = measurements Humidity (.humidityPercent)
      co2 = measurements CO2 (.co2Ppm)
      fields =
        ["temperatures" A..= temperatures | not (Map.null temperatures)]
          <> ["humidity" A..= humidity | not (Map.null humidity)]
          <> ["co2" A..= co2 | not (Map.null co2)]
   in if null fields then Nothing else Just $ A.object fields

{- | Reuse a manager across batches. Bound each request and reject redirects and
unexpected success codes: the local endpoint's success contract is 204.
-}
postHometrics :: HTTP.Manager -> HometricsConfig -> Map Text HometricsSensorConfig -> [SensorSample] -> IO ()
postHometrics manager cfg sensors samples = case hometricsPayload sensors samples of
  Nothing -> pure ()
  Just payload -> do
    when (T.null cfg.endpoint) $ throwIO $ userError "Hometrics endpoint must not be empty"
    base <- HTTP.parseRequest $ T.unpack cfg.endpoint
    let request =
          base
            { HTTP.method = "POST"
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            , HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode payload
            , HTTP.responseTimeout = HTTP.responseTimeoutMicro 10000000
            , HTTP.redirectCount = 0
            }
    response <- HTTP.httpNoBody request manager
    unless (statusCode (HTTP.responseStatus response) == 204) $
      throwIO $
        userError $
          "Hometrics returned HTTP " <> show (statusCode $ HTTP.responseStatus response)
