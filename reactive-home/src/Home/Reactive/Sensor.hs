{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.Sensor (
  SensorSample (..),
  SensorSnapshot,
  sensorSnapshotS,
  freshSamples,
  pendingAfterDelivery,
) where

import Data.Aeson (ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import FRP.Rhine (ClSF, Time, absoluteS, feedback, returnA)
import GHC.Generics (Generic)
import Home.Reactive.Duration (Duration (..))
import Network.SwitchBot.Advertisement (SensorReading)

data SensorSample = SensorSample
  { sensor :: !Text
  , observedAt :: !UTCTime
  , reading :: !SensorReading
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

type SensorSnapshot = Map Text SensorSample

-- | A clock tick with an empty batch still expires stale sensors.
sensorSnapshotS :: (Monad m, Time cl ~ UTCTime) => Duration -> ClSF m cl [SensorSample] SensorSnapshot
sensorSnapshotS ttl = feedback Map.empty proc (samples, previous) -> do
  now <- absoluteS -< ()
  let current = freshSamples ttl now $ Map.unionWith newest (Map.fromList [(s.sensor, s) | s <- samples]) previous
  returnA -< (current, current)
  where
    newest a b = if a.observedAt >= b.observedAt then a else b

freshSamples :: Duration -> UTCTime -> SensorSnapshot -> SensorSnapshot
freshSamples ttl now = Map.filter (\s -> diffUTCTime now s.observedAt < realToFrac ttl.seconds)

-- | Acknowledging a batch must not delete newer samples received during I/O.
pendingAfterDelivery :: SensorSnapshot -> SensorSnapshot -> SensorSnapshot
pendingAfterDelivery sent current = Map.differenceWith keepNewer current sent
  where
    keepNewer sample delivered = if sample == delivered then Nothing else Just sample
