{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.AutoLock (
  AutoLockEvent (..),
  autolockEventS,
  handleAutoLockEvent,
) where

import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Effectful (Eff, (:>))
import Effectful.Network.Mqtt (Mqtt, publish_)
import Effectful.Reader.Static (Reader, asks)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.ESPresense (Duration (..), Heartbeated (..))
import Home.Reactive.MQTT (MqttSnapshot (..))
import Home.Reactive.Sesame5

data AutoLockEvent = AutoLockEvent
  { name :: !T.Text
  , device :: !SesameDevice
  }
  deriving stock (Show, Eq, Ord, Generic)

data PendingAutoLock = PendingAutoLock
  { device :: !SesameDevice
  , deadline :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)

autolockEventS ::
  ( Reader SesameEnv :> es
  , Time cl ~ UTCTime
  ) =>
  ClSF (Eff es) cl (MqttSnapshot, Heartbeated SesameStatus) [AutoLockEvent]
autolockEventS = feedback HM.empty proc ((mqtt, input), pending) -> do
  TimeInfo {..} <- timeInfo -< ()
  devices <- constMCl (asks @SesameEnv (.devices)) -< ()
  let pendingEnabled = HM.filter (not . dismissed mqtt . (.device)) pending
      expired = HM.filter ((<= absolute) . (.deadline)) pendingEnabled
      active = HM.filter ((> absolute) . (.deadline)) pending
      expiredEvents =
        [ AutoLockEvent {name, device = item.device}
        | (name, item) <- HM.toList expired
        ]
  let !(events, pending') =
        case input of
          Heartbeat -> (expiredEvents, active)
          Event status
            | HM.member status.name active
            , status.lockCurrentState == LOCKED ->
                (expiredEvents, HM.delete status.name active)
            | HM.member status.name active -> (expiredEvents, active)
            | status.lockCurrentState == UNLOCKED
            , Just device <- HM.lookup status.name devices
            , Just timeout <- device.autolock_timeout ->
                if dismissed mqtt device
                  then (expiredEvents, active)
                  else
                    ( expiredEvents
                    , HM.insert status.name (PendingAutoLock device (addUTCTime (realToFrac timeout.seconds) absolute)) active
                    )
            | otherwise -> (expiredEvents, active)
  returnA -< (events, pending')

dismissed :: MqttSnapshot -> SesameDevice -> Bool
dismissed mqtt device =
  or
    [ mqtt.switches HM.!? condition.switch == Just True
    | condition <- device.autolock_dismiss
    ]

handleAutoLockEvent ::
  ( Mqtt :> es
  , Reader SesameEnv :> es
  ) =>
  [AutoLockEvent] -> Eff es ()
handleAutoLockEvent events = do
  prefix <- asks @SesameEnv (.prefix)
  for_ events \event ->
    publish_ (sesameCommandTopic prefix event.device) "LOCKED"
