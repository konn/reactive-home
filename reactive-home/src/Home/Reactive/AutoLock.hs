{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.AutoLock (
  AutoLockEvent (..),
  AutoLockLog (..),
  AutoLockOutput (..),
  autolockEventS,
  renderAutoLockLog,
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

data AutoLockLog
  = AutoLockTimerStarted !T.Text !UTCTime
  | AutoLockTimerCanceled !T.Text
  | AutoLockTimerKept !T.Text
  | AutoLockStartDismissed !T.Text
  | AutoLockFireDismissed !T.Text
  | AutoLockFired !T.Text
  deriving stock (Show, Eq, Ord, Generic)

data AutoLockOutput = AutoLockOutput
  { events :: ![AutoLockEvent]
  , logs :: ![AutoLockLog]
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
  ClSF (Eff es) cl (MqttSnapshot, Heartbeated SesameStatus) AutoLockOutput
autolockEventS = feedback HM.empty proc ((mqtt, input), pending) -> do
  TimeInfo {..} <- timeInfo -< ()
  devices <- constMCl (asks @SesameEnv (.devices)) -< ()
  let expiredPending = HM.filter ((<= absolute) . (.deadline)) pending
      expiredEnabled = HM.filter (not . dismissed mqtt . (.device)) expiredPending
      expiredDismissed = HM.filter (dismissed mqtt . (.device)) expiredPending
      active = HM.filter ((> absolute) . (.deadline)) pending
      expiredEvents =
        [ AutoLockEvent {name, device = item.device}
        | (name, item) <- HM.toList expiredEnabled
        ]
      expiredLogs =
        [ AutoLockFired name
        | name <- HM.keys expiredEnabled
        ]
          <> [ AutoLockFireDismissed name
             | name <- HM.keys expiredDismissed
             ]
      output logs = AutoLockOutput {events = expiredEvents, logs = expiredLogs <> logs}
      startTimer status device timeout =
        let deadline = addUTCTime (realToFrac timeout.seconds) absolute
         in ( output [AutoLockTimerStarted status.name deadline]
            , HM.insert status.name (PendingAutoLock device deadline) active
            )
      dismissStart status =
        (output [AutoLockStartDismissed status.name], active)
      cancelTimer status =
        (output [AutoLockTimerCanceled status.name], HM.delete status.name active)
      keepTimer status =
        (output [AutoLockTimerKept status.name], active)
  let !(result, pending') =
        case input of
          Heartbeat -> (output [], active)
          Event status
            | HM.member status.name active
            , status.lockCurrentState == LOCKED ->
                cancelTimer status
            | HM.member status.name active
            , status.lockCurrentState == UNLOCKED ->
                keepTimer status
            | status.lockCurrentState == UNLOCKED
            , Just device <- HM.lookup status.name devices
            , Just timeout <- device.autolock_timeout ->
                if dismissed mqtt device
                  then dismissStart status
                  else startTimer status device timeout
            | otherwise -> (output [], active)
  returnA -< (result, pending')

renderAutoLockLog :: AutoLockLog -> T.Text
renderAutoLockLog = \case
  AutoLockTimerStarted name deadline ->
    "AutoLock timer started: device="
      <> name
      <> ", deadline="
      <> T.pack (show deadline)
  AutoLockTimerCanceled name ->
    "AutoLock timer canceled by LOCKED status: device=" <> name
  AutoLockTimerKept name ->
    "AutoLock timer kept after repeated UNLOCKED status: device=" <> name
  AutoLockStartDismissed name ->
    "AutoLock not started because dismissal switch is on: device=" <> name
  AutoLockFireDismissed name ->
    "AutoLock suppressed at deadline because dismissal switch is on: device=" <> name
  AutoLockFired name ->
    "AutoLock fired: publishing LOCKED command for device=" <> name

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
