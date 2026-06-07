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

module Home.Reactive.Utils (
  MovingAverageConfig (..),
  movingAverageS,
  Spanned (..),
  spanned,
  effReaderS,
  movingAverageS_,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader qualified as MTL
import Data.Aeson (FromJSON, ToJSON)
import Data.Sequence qualified as Seq
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as EffR
import FRP.Rhine
import GHC.Generics (Generic)

data MovingAverageConfig diff = MovingAverageConfig
  { timeout :: !(Maybe diff)
  , window :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

movingAverageS ::
  (Fractional a, Monad m, Ord (Diff (Time cl)), TimeDomain (Time cl)) =>
  ClSF m cl (MovingAverageConfig (Diff (Time cl)), a) (Maybe a)
movingAverageS = feedback Seq.empty proc ((cfg, x), hist) -> do
  TimeInfo {..} <- timeInfo -< ()
  let !window = max 1 cfg.window
      !eligible
        | Just timeout <- cfg.timeout = \(_, stamp) ->
            sinceInit `difference` stamp <= timeout
        | otherwise = const True
      !hist'
        | maybe False (sinceLast >) cfg.timeout = Seq.singleton (x, sinceInit)
        | otherwise = Seq.take window $ (x, sinceInit) Seq.<| Seq.filter eligible hist
      size = Seq.length hist'
      !avg =
        if size < window
          then Nothing
          else Just (sum (fst <$> hist') / fromIntegral size)
  returnA -< (avg, hist')

movingAverageS_ ::
  (Fractional a, Monad m, Ord (Diff (Time cl)), TimeDomain (Time cl)) =>
  MovingAverageConfig (Diff (Time cl)) ->
  ClSF m cl a (Maybe a)
movingAverageS_ cfg = feedback Seq.empty proc (x, hist) -> do
  TimeInfo {..} <- timeInfo -< ()
  let !window = max 1 cfg.window
      !eligible
        | Just timeout <- cfg.timeout = \(_, stamp) ->
            sinceInit `difference` stamp <= timeout
        | otherwise = const True
      !hist'
        | maybe False (sinceLast >) cfg.timeout = Seq.singleton (x, sinceInit)
        | otherwise = Seq.take window $ (x, sinceInit) Seq.<| Seq.filter eligible hist
      size = Seq.length hist'
      !avg =
        if size < window
          then Nothing
          else Just (sum (fst <$> hist') / fromIntegral size)
  returnA -< (avg, hist')

data Spanned t a = Spanned {value :: !a, duration :: !t}
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (FromJSON, ToJSON)

spanned ::
  (Eq a, Monad m, Num (Diff (Time cl)), TimeDomain (Time cl)) =>
  ClSF m cl a (Spanned (Diff (Time cl)) a)
spanned = feedback Nothing proc (x, prev) -> do
  TimeInfo {..} <- timeInfo -< ()
  case prev of
    Just (x0, started)
      | x0 == x -> returnA -< (Spanned {value = x, duration = absolute `diffTime` started}, Just (x, started))
    _ -> returnA -< (Spanned {value = x, duration = 0}, Just (x, absolute))

effReaderS ::
  forall r es cl a b.
  (Reader r :> es) =>
  ClSF (Eff es) cl (a, r) b ->
  ClSF (Eff es) cl a b
effReaderS =
  hoistS (\act -> MTL.runReaderT (commuteReaders act) =<< lift EffR.ask)
    . readerS
