{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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

-- TODO: Use more fine-grained infor source than monolichic 'ESPresenseSnapshot'.

data UnlockEvent = Unlock
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)

data UnlockConfig = UnlockConfig
  { room :: !T.Text
  , delay :: !Duration
  , locks :: ![T.Text]
  , approach :: [ApproachCondition]
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)

instance Toml.HasItemCodec UnlockConfig where
  hasItemCodec = Right genericCodec

instance Toml.HasItemCodec ApproachCondition where
  hasItemCodec = Right genericCodec

instance Toml.HasCodec UnlockConfig where
  hasCodec = Toml.table genericCodec

data ApproachCondition = ApproachCondition
  { sensor :: !ESPSensorName
  , distance :: !Float
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey)

isRoomOccupied ::
  (Reader UnlockConfig :> es) =>
  ClSF (Eff es) cl ESPresenseSnapshot Bool
isRoomOccupied = proc snapshot -> do
  roomName <- constMCl (asks @UnlockConfig (.room)) -< ()
  returnA -< maybe False (not . null) (HM.lookup roomName snapshot.rooms)

data UnlockStatus
  = -- | Occupied by at least one device
    Occupied
  | -- | Empty and waiting for the specified delay to pass
    Waiting
  | Vacant
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

-- | Emits 'Unlock' when the room becomes occupied after vacant for at least the specified delay.
unlockEventS ::
  ( Reader UnlockConfig :> es
  , Time cl ~ UTCTime
  ) =>
  ClSF (Eff es) cl ESPresenseSnapshot (Maybe UnlockEvent)
unlockEventS =
  isRoomOccupied >-> spanned >-> feedback Waiting proc (Spanned {value = occupied, ..}, prev) -> do
    thresh <- constMCl (asks @UnlockConfig (.delay)) -< ()
    returnA
      -<
        if
          | occupied ->
              case prev of
                Waiting -> (Just Unlock, Occupied)
                _ -> (Nothing, Occupied)
          | duration >= thresh.seconds -> (Nothing, Vacant)
          | otherwise -> (Nothing, Waiting)

handleUnlockEvent ::
  ( Mqtt :> es
  , Reader UnlockConfig :> es
  , Reader SesameConfig :> es
  ) =>
  UnlockEvent -> Eff es ()
handleUnlockEvent Unlock = do
  locks <- asks @UnlockConfig (.locks)
  sesames <- asks @SesameConfig (.devices)
  prefix <- asks @SesameConfig (.prefix)
  for_ locks \lock -> do
    case HM.lookup lock sesames of
      Nothing -> pure () -- TODO: Log the error.
      Just dev -> do
        let topic = Topic prefix <> Topic dev.uuid.raw <> "set"
        publish_ topic "UNLOCK"
