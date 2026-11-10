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
  ConfirmationCondition (..),
  DismissCondition (..),
  UnlockStatus (..),
  UnlockFeedback (..),
  unlockFeedbackS,
  unlockEventS,
  handleUnlockEvent,
  validateUnlockSensors,
) where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Aeson qualified as A
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
import Toml ((.=))
import Toml qualified
import Validation (Validation (..))

-- TODO: Disable the unlock during the night shift.

data UnlockEvent = Unlock
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)

data UnlockConfig = UnlockConfig
  { room :: !T.Text
  , delay :: !Duration
  , locks :: ![T.Text]
  , approach :: [ApproachCondition]
  , confirm :: [ConfirmationCondition]
  -- ^ All additional conditions must hold alongside an approach. Empty disables confirmation.
  , dismiss :: [DismissCondition]
  {- ^ Switches that prevent new vacancy qualification. Clearing the last active
  switch restarts room absence confirmation and the vacancy delay.
  -}
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, ToJSONKey)

instance Toml.HasCodec UnlockConfig where
  hasCodec = Toml.table unlockConfigCodec

instance Toml.HasItemCodec UnlockConfig where
  hasItemCodec = Right unlockConfigCodec

unlockConfigCodec :: Toml.TomlCodec UnlockConfig
unlockConfigCodec = do
  room <- Toml.text "room" .= (.room)
  unlockDelay <- Toml.hasCodec "delay" .= (.delay)
  locks <- Toml.hasCodec "locks" .= (.locks)
  approach <- Toml.hasCodec "approach" .= (.approach)
  confirm <- confirmationListCodec .= (.confirm)
  dismiss <- Toml.hasCodec "dismiss" .= (.dismiss)
  pure UnlockConfig {delay = unlockDelay, ..}

-- tomland's table-list codec omits empty lists when encoding, so decodeExact
-- rejects an explicit `confirm = []`. Preserve that representation, and accept
-- only an actual empty array (never a malformed value) as the disabled setting.
confirmationListCodec :: Toml.TomlCodec [ConfirmationCondition]
confirmationListCodec =
  Toml.Codec
    { Toml.codecRead = \toml ->
        if HM.member "confirm" toml.tomlPairs
          then [] <$ Toml.codecRead emptyCodec toml
          else Toml.codecRead listCodec toml
    , Toml.codecWrite = \conditions ->
        if null conditions
          then [] <$ Toml.codecWrite emptyCodec []
          else Toml.codecWrite listCodec conditions
    }
  where
    emptyCodec = Toml.hardcoded ([] :: [T.Text]) (Toml._Array Toml._Text) "confirm"
    listCodec = Toml.list confirmationCodec "confirm"

instance FromJSON UnlockConfig where
  parseJSON = A.withObject "UnlockConfig" \obj ->
    UnlockConfig
      <$> obj A..: "room"
      <*> obj A..: "delay"
      <*> obj A..: "locks"
      <*> obj A..: "approach"
      <*> obj A..:? "confirm" A..!= []
      <*> obj A..: "dismiss"

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

{- | References the same sensor/device definitions as an approach, with an
explicit freshness limit. No sensor has a special role in the implementation.
-}
data ConfirmationCondition = ConfirmationCondition
  { sensor :: !ESPSensorName
  , device :: !ESPDeviceId
  , distance :: !Float
  , max_age :: !Duration
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, ToJSONKey)

instance FromJSON ConfirmationCondition where
  parseJSON value = do
    condition <- A.genericParseJSON A.defaultOptions value
    either (fail . T.unpack . snd) pure (validateConfirmation condition)

instance Toml.HasCodec ConfirmationCondition where
  hasCodec = Toml.table confirmationCodec

instance Toml.HasItemCodec ConfirmationCondition where
  hasItemCodec = Right confirmationCodec

confirmationCodec :: Toml.TomlCodec ConfirmationCondition
confirmationCodec =
  Toml.Codec
    { Toml.codecRead = \toml ->
        case Toml.codecRead baseCodec toml of
          Failure errors -> Failure errors
          Success condition ->
            case validateConfirmation condition of
              Left (key, message) -> Failure [Toml.BiMapError key (Toml.ArbitraryError message)]
              Right valid -> Success valid
    , Toml.codecWrite = Toml.codecWrite baseCodec
    }
  where
    baseCodec = Toml.genericCodec @ConfirmationCondition

validateConfirmation :: ConfirmationCondition -> Either (Toml.Key, T.Text) ConfirmationCondition
validateConfirmation condition
  | not (positiveFinite condition.distance) = Left ("distance", "distance must be finite and positive")
  | not (positiveFinite condition.max_age.seconds) = Left ("max_age", "max_age must be a finite, positive duration")
  | otherwise = Right condition
  where
    positiveFinite :: (RealFloat a) => a -> Bool
    positiveFinite value = value > 0 && not (isNaN value || isInfinite value)

-- | Catch misspelled confirmation references before starting any automation.
validateUnlockSensors :: Maybe ESPresenseConfig -> UnlockConfig -> Either T.Text ()
validateUnlockSensors esp cfg = for_ cfg.confirm \condition -> do
  _ <- either (Left . snd) Right (validateConfirmation condition)
  if condition.sensor `elem` maybe [] (map (.name) . (.sensors)) esp
    then pure ()
    else Left $ "Unknown unlock.confirm sensor: " <> T.show condition.sensor
  if condition.device `elem` maybe [] (.devices) esp
    then pure ()
    else Left $ "Unknown unlock.confirm device: " <> T.show condition.device

confirmationSatisfied :: UTCTime -> ESPresenseSnapshot -> ConfirmationCondition -> Bool
confirmationSatisfied now snapshot condition =
  case HM.lookup condition.device =<< HM.lookup condition.sensor snapshot.sensors of
    Nothing -> False
    Just observation ->
      let age = diffTime now observation.timestamp
       in observation.distance <= condition.distance
            && age >= 0
            && age < condition.max_age.seconds

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
  | -- | Qualified vacancy, preserved until confirmed approach or dismissal is cleared
    Vacant
  | -- | Qualified vacancy with room presence, waiting for a confirmed approach
    ReadyForUnlock
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

data UnlockFeedback = UnlockFeedback
  { near :: !Bool
  , confirmed :: !Bool
  -- ^ All additional conditions hold; True when no confirmation is configured.
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
    confirmation <- constMCl (asks @UnlockConfig (.confirm)) -< ()
    TimeInfo {absolute} <- timeInfo -< ()
    let !confirmed = all (confirmationSatisfied absolute snapshot) confirmation
        !dismiss =
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
                  Vacant; ReadyForUnlock
                    | confirmed -> (Just Unlock, Occupied)
                    | otherwise -> (Nothing, if occupied then ReadyForUnlock else Vacant)
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
        !fb = UnlockFeedback {near, confirmed, occupied, rechecking, duration, status}
    returnA -< ((fb, event), (status, dismiss, deadline))

{- | Emits 'Unlock' on the first confirmed approach after a qualified vacancy.
Unconfirmed approaches preserve qualification and are reconsidered every heartbeat;
confirmation alone never triggers an unlock. Raw approach still prevents vacancy.
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
