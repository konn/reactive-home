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
  movingAverage,
  movingAverageTimeout,
  Spanned (..),
  spanned,
) where

import Data.Sequence qualified as Seq
import FRP.Rhine
import GHC.Generics (Generic)

movingAverage :: (Fractional a, Monad m) => Int -> ClSF m cl a a
movingAverage n
  | n <= 0 = error $ "movingAverage: n must be positive, but got: " <> show n
  | otherwise = feedback Seq.empty proc (x, hist) -> do
      let !hist' = Seq.take n (x Seq.<| hist)
          size = Seq.length hist'
          !avg = sum hist' / fromIntegral size
      returnA -< (avg, hist')

movingAverageTimeout ::
  (Fractional a, Monad m, Ord (Diff (Time cl))) =>
  Diff (Time cl) ->
  Int ->
  ClSF m cl a (Maybe a)
movingAverageTimeout timeout n
  | n <= 0 = error $ "movingAverageTimeout: n must be positive, but got: " <> show n
  | otherwise = feedback Seq.empty proc (x, hist) -> do
      TimeInfo {..} <- timeInfo -< ()
      let !hist'
            | sinceLast > timeout = Seq.singleton x
            | otherwise = Seq.take n $ x Seq.<| hist
          size = Seq.length hist'
          !avg = if size == 0 then Nothing else Just (sum hist' / fromIntegral size)
      returnA -< (avg, hist')

data Spanned t a = Spanned {value :: !a, duration :: !t}
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

spanned ::
  (Eq a, Monad m, Num (Diff (Time cl)), Clock m cl) =>
  ClSF m cl a (Spanned (Diff (Time cl)) a)
spanned = feedback Nothing proc (x, prev) -> do
  TimeInfo {..} <- timeInfo -< ()
  case prev of
    Just (x0, started)
      | x0 == x -> returnA -< (Spanned {value = x, duration = absolute `diffTime` started}, Just (x, started))
    _ -> returnA -< (Spanned {value = x, duration = 0}, Just (x, absolute))
