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
  | Vacant
  | ReadyForUnlock
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

data UnlockFeedback = UnlockFeedback
  { near :: !Bool
  , occupied :: !Bool
  , duration :: !(Diff UTCTime)
  , status :: !UnlockStatus
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

-- TODO: Use more fine-grained infor source than monolichic 'ESPresenseSnapshot'.
unlockFeedbackS ::
  ( Reader UnlockConfig :> es
  , Time cl ~ UTCTime
  ) =>
  ClSF (Eff es) cl (MqttSnapshot, ESPresenseSnapshot) (UnlockFeedback, Maybe UnlockEvent)
unlockFeedbackS =
  (second (anyApproachDetected &&& isRoomOccupied >-> spanned)) >-> feedback Waiting proc ((mqtt, (near, Spanned {value = occupied, ..})), prev) -> do
    thresh <- constMCl (asks @UnlockConfig (.delay)) -< ()
    dismissal <- constMCl (asks @UnlockConfig (.dismiss)) -< ()
    let !dismiss =
          or
            [ mqtt.switches HM.!? sw.switch == Just True
            | sw <- dismissal
            ]
        !unlockCmd
          | not dismiss = Just Unlock
          | otherwise = Nothing
        !next =
          if
            | near ->
                case prev of
                  Vacant; ReadyForUnlock -> (unlockCmd, Occupied)
                  Waiting; Occupied -> (Nothing, Occupied)
            | occupied ->
                case prev of
                  (Vacant; ReadyForUnlock)
                    | not near -> (Nothing, ReadyForUnlock)
                    | otherwise -> (unlockCmd, Occupied)
                  _ -> (Nothing, Occupied)
            | ReadyForUnlock <- prev -> (Nothing, Vacant)
            | duration >= thresh.seconds, not dismiss -> (Nothing, Vacant)
            | otherwise -> (Nothing, Waiting)
        !(event, status) = next
        !fb = UnlockFeedback {near, occupied, duration, status}
    returnA -< ((fb, event), status)

-- | Emits 'Unlock' when the room becomes occupied after vacant for at least the specified delay.
unlockEventS ::
  ( Reader UnlockConfig :> es
  , Time cl ~ UTCTime
  ) =>
  ClSF (Eff es) cl (MqttSnapshot, ESPresenseSnapshot) (Maybe UnlockEvent)
unlockEventS = proc snapshot -> do
  (_, event) <- unlockFeedbackS -< snapshot
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
        let topic = Topic prefix <> Topic dev.uuid.raw <> "set"
        publish_ topic "UNLOCKED"
