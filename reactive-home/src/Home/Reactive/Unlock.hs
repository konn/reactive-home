{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.Unlock (
  UnlockConfig (..),
  UnlockEvent (..),
  ApproachCondition (..),
  DismissCondition (..),
  UnlockStatus (..),
  UnlockFeedback (..),
  unlockFeedbackS,
  unlockEventS,
  handleUnlockEvent,
) where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Effectful (Eff, (:>))
import Effectful.Network.Mqtt (Mqtt, publish_)
import Effectful.Reader.Static (Reader, asks)
import FRP.Rhine
import GHC.Generics
import Home.Reactive.ESPresense
import Home.Reactive.MQTT
import Home.Reactive.Sesame5
import Home.Reactive.Utils
import Toml qualified
import Toml.Codec.Generic

-- TODO: Disable the unlock during the night shift.

data UnlockEvent = Unlock
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)

data UnlockConfig = UnlockConfig
  { room :: !T.Text
  , delay :: !Duration
  , locks :: ![T.Text]
  , approach :: [ApproachCondition]
  , dismiss :: [DismissCondition]
  {- ^ Switches that prevent new vacancy qualification. Clearing the last active
  switch restarts room absence confirmation and the vacancy delay.
  -}
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)
  deriving (Toml.HasItemCodec, Toml.HasCodec) via Toml.TomlTable UnlockConfig

data ApproachCondition = ApproachCondition
  { sensor :: !ESPSensorName
  , device :: !ESPDeviceId
  , distance :: !Float
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)
  deriving (Toml.HasItemCodec, Toml.HasCodec) via Toml.TomlTable ApproachCondition

data DismissCondition = DismissCondition {switch :: !T.Text}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)
  deriving (Toml.HasItemCodec, Toml.HasCodec) via Toml.TomlTable DismissCondition

-- TODO: Use more fine-grained infor source than monolichic 'ESPresenseSnapshot'.

isRoomOccupied ::
  (Reader UnlockConfig :> es) =>
  ClSF (Eff es) cl ESPresenseSnapshot Bool
isRoomOccupied = proc snapshot -> do
  roomName <- constMCl (asks @UnlockConfig (.room)) -< ()
  returnA -< maybe False (not . null) (HM.lookup roomName snapshot.rooms)

anyApproachDetected ::
  (Reader UnlockConfig :> es) =>
  ClSF (Eff es) cl ESPresenseSnapshot Bool
anyApproachDetected = proc snapshot -> do
  conditions <- constMCl (asks @UnlockConfig (.approach)) -< ()
  or <$> parallely singleApproach -< map (,snapshot) conditions

-- FIXME: use moving average value!
singleApproach :: ClSF (Eff es) cl (ApproachCondition, ESPresenseSnapshot) Bool
singleApproach = proc (cond, snapshot) -> do
  let sensorName = cond.sensor
      thresh = cond.distance
  returnA
    -< case HM.lookup cond.device =<< HM.lookup sensorName snapshot.sensors of
      Nothing -> False
      Just sensor -> sensor.distance <= thresh

data UnlockStatus
  = -- | Occupied by at least one device
    Occupied
  | -- | Empty and waiting for the specified delay to pass
    Waiting
  | -- | Qualified vacancy, preserved until approach or dismissal is cleared
    Vacant
  | -- | Qualified vacancy with room presence, still waiting for an approach
    ReadyForUnlock
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

data UnlockFeedback = UnlockFeedback
  { near :: !Bool
  , occupied :: !Bool
  , rechecking :: !Bool
  -- ^ Waiting for a fresh room timeout after dismissal was cleared.
  , duration :: !(Diff UTCTime)
  -- ^ Eligible vacancy duration, excluding dismissal and room rechecking.
  , status :: !UnlockStatus
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

-- TODO: Use more fine-grained infor source than monolichic 'ESPresenseSnapshot'.
unlockFeedbackS ::
  ( Reader UnlockConfig :> es
  , Time cl ~ UTCTime
  ) =>
  -- | Presence timeout of the configured unlock room.
  Duration ->
  ClSF (Eff es) cl (MqttSnapshot, ESPresenseSnapshot) (UnlockFeedback, Maybe UnlockEvent)
unlockFeedbackS roomTimeout =
  feedback (Waiting, False, Nothing) proc ((mqtt, snapshot), (previousStatus, previousDismissal, previousDeadline)) -> do
    (near, occupied) <- anyApproachDetected &&& isRoomOccupied -< snapshot
    thresh <- constMCl (asks @UnlockConfig (.delay)) -< ()
    dismissal <- constMCl (asks @UnlockConfig (.dismiss)) -< ()
    TimeInfo {absolute} <- timeInfo -< ()
    let !dismiss =
          or
            [ mqtt.switches HM.!? sw.switch == Just True
            | sw <- dismissal
            ]
        !released = previousDismissal && not dismiss
        !deadline =
          if released
            then Just $ addUTCTime (realToFrac roomTimeout.seconds) absolute
            else previousDeadline
        !rechecking = maybe False (absolute <) deadline
        !prev = if released then Waiting else previousStatus
        !eligible = not $ near || occupied || dismiss
    Spanned {duration = vacancyDuration} <- spanned -< eligible
    -- Clip the observed vacancy to the fresh room deadline. Even if heartbeats
    -- are delayed, time before this deadline cannot count toward unlock.delay.
    let !duration
          | not eligible = 0
          | otherwise = maybe vacancyDuration (min vacancyDuration . max 0 . diffTime absolute) deadline
        !next =
          if
            | near ->
                case prev of
                  Vacant; ReadyForUnlock -> (Just Unlock, Occupied)
                  Waiting; Occupied -> (Nothing, Occupied)
            | occupied ->
                case prev of
                  Vacant; ReadyForUnlock -> (Nothing, ReadyForUnlock)
                  _ -> (Nothing, Occupied)
            | (ReadyForUnlock; Vacant) <- prev -> (Nothing, Vacant)
            -- Turning dismissal on preserves qualification; turning it off
            -- resets prev above and requires both timers to run afresh.
            | duration >= thresh.seconds, not dismiss, not rechecking -> (Nothing, Vacant)
            | otherwise -> (Nothing, Waiting)
        !(event, status) = next
        !fb = UnlockFeedback {near, occupied, rechecking, duration, status}
    returnA -< ((fb, event), (status, dismiss, deadline))

{- | Emits 'Unlock' on the first approach after a qualified vacancy.
Turning dismissal on preserves a qualified vacancy. Clearing the last active
dismissal switch revokes it and restarts room absence confirmation followed by
the vacancy delay. Sensor observations retain their original timestamps.
-}
unlockEventS ::
  ( Reader UnlockConfig :> es
  , Time cl ~ UTCTime
  ) =>
  -- | Presence timeout of the configured unlock room.
  Duration ->
  ClSF (Eff es) cl (MqttSnapshot, ESPresenseSnapshot) (Maybe UnlockEvent)
unlockEventS roomTimeout = proc snapshot -> do
  (_, event) <- unlockFeedbackS roomTimeout -< snapshot
  returnA -< event

handleUnlockEvent ::
  ( Mqtt :> es
  , Reader UnlockConfig :> es
  , Reader SesameEnv :> es
  ) =>
  UnlockEvent -> Eff es ()
handleUnlockEvent Unlock = do
  locks <- asks @UnlockConfig (.locks)
  sesames <- asks @SesameEnv (.devices)
  prefix <- asks @SesameEnv (.prefix)
  for_ locks \lock -> do
    case HM.lookup lock sesames of
      Nothing -> pure () -- TODO: Log the error.
      Just dev -> do
        let topic = sesameCommandTopic prefix dev
        publish_ topic "UNLOCKED"
