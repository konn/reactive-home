{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.Duration (
  Duration (..),
  millis,
  seconds,
  minutes,
  hours,
  days,
  formatDuration,
  parseDuration,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Char qualified as C
import Data.Hashable (Hashable)
import Data.Text qualified as T
import FRP.Rhine (Diff, UTCTime)
import GHC.Generics (Generic)
import Text.Read (readEither)
import Toml (HasCodec (..), textBy)

newtype Duration = Duration {seconds :: Diff UTCTime}
  deriving (Eq, Ord, Generic)
  deriving newtype (Hashable)

instance ToJSON Duration where
  toJSON = A.toJSON . formatDuration
  toEncoding (Duration secs) = A.toEncoding (formatDuration (Duration secs))

instance FromJSON Duration where
  parseJSON v = do
    txt <- A.parseJSON v
    either (fail . T.unpack) pure (parseDuration txt)

millis :: Double -> Duration
millis ms = Duration (ms / 1000)

seconds :: Double -> Duration
seconds = Duration

minutes :: Double -> Duration
minutes m = Duration (m * 60)

hours :: Double -> Duration
hours h = Duration (h * 3600)

days :: Double -> Duration
days d = Duration (d * 86400)

instance Show Duration where
  show (Duration secs) = show secs <> "s"

instance HasCodec Duration where
  hasCodec = textBy formatDuration parseDuration

formatDuration :: Duration -> T.Text
formatDuration (Duration secs)
  | secs >= 24 * 3600 = T.pack (show $ secs / (24 * 3600)) <> "d"
  | secs >= 3600 = T.pack (show $ secs / 3600) <> "h"
  | secs >= 60 = T.pack (show $ secs / 60) <> "m"
  | secs >= 1 = T.pack (show secs) <> "s"
  | otherwise = T.pack (show $ secs * 1000) <> "ms"

parseDuration :: T.Text -> Either T.Text Duration
parseDuration inp = case T.span (\c -> C.isDigit c || c == '.' || c == '_') inp of
  ("", _) -> Left $ "Invalid duration format: empty string"
  (numPart, T.strip -> rest) ->
    case readEither (T.unpack numPart) of
      Left err -> Left $ "Invalid duration (bare seconds, or real number with suffix ms/s/m/h/d expected): " <> T.pack err
      Right num ->
        if T.null rest
          then Right $ Duration num
          else case T.toLower rest of
            "ms" -> Right $ Duration (num / 1000)
            "s" -> Right $ Duration num
            "m" -> Right $ Duration (num * 60)
            "h" -> Right $ Duration (num * 3600)
            "d" -> Right $ Duration (num * 86400)
            _ -> Left $ "Invalid duration suffix: expected no suffix (treated as second), or one of ms/s/m/h/d, but got: " <> rest
