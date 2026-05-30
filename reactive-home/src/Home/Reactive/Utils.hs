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
) where

import Data.Sequence qualified as Seq
import FRP.Rhine

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
  ClSF m cl a a
movingAverageTimeout timeout n
  | n <= 0 = error $ "movingAverageTimeout: n must be positive, but got: " <> show n
  | otherwise = feedback Seq.empty proc (x, hist) -> do
      TimeInfo {..} <- timeInfo -< ()
      let !hist'
            | sinceLast > timeout = Seq.singleton x
            | otherwise = Seq.take n $ x Seq.<| hist
          size = Seq.length hist'
          !avg = sum hist' / fromIntegral size
      returnA -< (avg, hist')
