{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.ScheduledSwitch (
  ScheduledSwitchEvent (..),
  ScheduledSwitchPublishError (..),
  scheduledSwitchEventsS,
  scheduledSwitchEventsWithS,
  publishScheduledSwitchEvents,
) where

import Control.Exception.Safe (Exception, throwIO)
import Data.Time (diffUTCTime)
import Effectful (Eff, (:>))
import Effectful.Network.Mqtt (Mqtt, PublishOptions (..), PublishResult (..), QoS (..), ReasonCode, defaultPublishOptions, isSuccess, publish)
import FRP.Rhine (ClSF, Time, UTCTime, absoluteS, arrMCl, feedback, returnA, sinceInitS)
import GHC.Generics (Generic)
import Home.Reactive.Duration (Duration (..))
import Home.Reactive.MQTT (MqttScheduledSwitch (..))

data ScheduledSwitchEvent = ScheduledSwitchEvent
  { switch :: !MqttScheduledSwitch
  , state :: !Bool
  }
  deriving stock (Show, Eq, Ord, Generic)

data ScheduledSwitchPublishError = ScheduledSwitchPublishError !ScheduledSwitchEvent !ReasonCode
  deriving stock (Show, Eq, Generic)

instance Exception ScheduledSwitchPublishError

data SwitchTimer
  = Off !Double
  | On !Double !Double

{- | Start off, then activate on the interval cadence measured from clock
initialization. A late activation still gets its full on duration. Missed
activations are skipped so a delayed heartbeat cannot cause a burst of pulses.
-}
scheduledSwitchEventsS ::
  (Time cl ~ UTCTime) =>
  [MqttScheduledSwitch] ->
  ClSF (Eff es) cl () [ScheduledSwitchEvent]
scheduledSwitchEventsS = scheduledSwitchEventsWithS (\now _ -> pure now)

{- | The publishing callback returns the time its publications completed. Start
the ON duration from that time so a blocked publish cannot shorten a pulse.
-}
scheduledSwitchEventsWithS ::
  (Time cl ~ UTCTime) =>
  (UTCTime -> [ScheduledSwitchEvent] -> Eff es UTCTime) ->
  [MqttScheduledSwitch] ->
  ClSF (Eff es) cl () [ScheduledSwitchEvent]
scheduledSwitchEventsWithS emit switches =
  feedback (Nothing <$ switches) proc ((), timers) -> do
    now <- sinceInitS -< ()
    timestamp <- absoluteS -< ()
    let results = zipWith (stepSwitch now) switches timers
        events = concatMap fst results
    completedAt <- arrMCl (uncurry emit) -< (timestamp, events)
    let completed = now + max 0 (realToFrac $ diffUTCTime completedAt timestamp)
        timers' = zipWith (completeSwitch completed) switches results
    returnA -< (events, timers')

completeSwitch :: Double -> MqttScheduledSwitch -> ([ScheduledSwitchEvent], Maybe SwitchTimer) -> Maybe SwitchTimer
completeSwitch completed switch (events, timer)
  | any (.state) events = Just $ On (nextActivation switch completed) (completed + switch.on_duration.seconds)
  | not (null events)
  , Just (Off nextOn) <- timer
  , nextOn <= completed =
      Just $ Off $ nextActivation switch completed
  | otherwise = timer

stepSwitch :: Double -> MqttScheduledSwitch -> Maybe SwitchTimer -> ([ScheduledSwitchEvent], Maybe SwitchTimer)
stepSwitch now switch timer =
  case timer of
    Nothing ->
      let (events, timer') = stepSwitch now switch $ Just $ Off switch.interval.seconds
       in (ScheduledSwitchEvent switch False : events, timer')
    Just (Off nextOn)
      | now >= nextOn ->
          ( [ScheduledSwitchEvent switch True]
          , Just $ On (nextActivation switch now) (now + switch.on_duration.seconds)
          )
    Just (On nextOn offAt)
      | now >= offAt ->
          ( [ScheduledSwitchEvent switch False]
          , Just $ Off $ if nextOn > now then nextOn else nextActivation switch now
          )
    _ -> ([], timer)

nextActivation :: MqttScheduledSwitch -> Double -> Double
nextActivation switch now =
  fromInteger (floor (now / switch.interval.seconds) + 1) * switch.interval.seconds

-- | Publish the current state for both live and later subscribers.
publishScheduledSwitchEvents :: (Mqtt :> es) => [ScheduledSwitchEvent] -> Eff es ()
publishScheduledSwitchEvents = mapM_ \event -> do
  result <-
    publish
      event.switch.topic
      (if event.state then "true" else "false")
      defaultPublishOptions {qos = QoS1, retain = True}
  case result of
    AckedQoS1 reason _ | not (isSuccess reason) -> throwIO $ ScheduledSwitchPublishError event reason
    AckedQoS2 reason _ | not (isSuccess reason) -> throwIO $ ScheduledSwitchPublishError event reason
    _ -> pure ()
