{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Home.Reactive.Metrics.Mackerel (
  MackerelConfig (..),
  postMackerelS,
  postMackerel,
  ToMackerelMetrics (..),
  MackerelMetrics (..),
) where

import Control.Exception (displayException)
import Control.Exception.Safe (tryAny)
import Control.Lens ((&), (.~))
import Data.Aeson (ToJSON)
import Data.Aeson qualified as A
import Data.DList qualified as DL
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effectful
import Effectful.Concurrent (Concurrent, forkIO)
import Effectful.Console.ByteString (Console)
import Effectful.Console.ByteString qualified as Eff
import Effectful.Reader.Static (Reader, ask)
import Effectful.Wreq (Wreq, postWith)
import Effectful.Wreq qualified as W
import FRP.Rhine
import GHC.Generics (Generic)
import Toml (HasCodec, HasItemCodec, TomlTable (..))

data MackerelConfig = MackerelConfig
  { service :: !T.Text
  , apiKey :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasItemCodec, HasCodec) via TomlTable MackerelConfig

postMackerelS ::
  ( Wreq :> es
  , Reader MackerelConfig :> es
  , Concurrent :> es
  , ToMackerelMetrics a
  , Console :> es
  ) =>
  ClSF (Eff es) cl a ()
postMackerelS = proc stt -> do
  cfg <- constMCl (ask @MackerelConfig) -< ()
  arrMCl (uncurry postMackerel) -< (cfg, stt)

data MackerelMetrics = MackerelEntry
  { name :: !T.Text
  , time :: !UTCTime
  , value :: !A.Value
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MackerelMetrics where
  toJSON (MackerelEntry {..}) =
    A.object
      [ "name" A..= name
      , "time" A..= floor @_ @Int (utcTimeToPOSIXSeconds time)
      , "value" A..= value
      ]

instance ToMackerelMetrics MackerelMetrics where
  toMetrics = pure

class ToMackerelMetrics a where
  toMetrics :: a -> [MackerelMetrics]

instance (ToMackerelMetrics a) => ToMackerelMetrics (Maybe a) where
  toMetrics = maybe [] toMetrics

instance (ToMackerelMetrics a) => ToMackerelMetrics [a] where
  toMetrics = DL.toList . foldMap (DL.fromList . toMetrics)

postMackerel ::
  ( Concurrent :> es
  , Wreq :> es
  , ToMackerelMetrics s
  , Console :> es
  ) =>
  MackerelConfig -> s -> Eff es ()
postMackerel MackerelConfig {..} s = do
  let metrics = toMetrics s
  case metrics of
    [] -> pure ()
    _ -> do
      let url = "https://api.mackerelio.com/api/v0/services/" <> T.unpack service <> "/tsdb"
          opts =
            W.defaults
              & W.header "X-Api-Key" .~ [TE.encodeUtf8 apiKey]
              & W.header "Content-Type" .~ ["application/json"]
      void $ forkIO $ tryAnyReport $ postWith opts url $ A.encode metrics

tryAnyReport :: (Console :> es) => Eff es a -> Eff es ()
tryAnyReport act = do
  tryAny act >>= \case
    Left err -> Eff.putStrLn $ TE.encodeUtf8 $ T.pack $ "Failed to post to Mackerel: " <> displayException err
    Right _ -> pure ()
