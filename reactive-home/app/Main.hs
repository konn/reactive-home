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

module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception (Exception, throwIO)
import Control.Lens (view)
import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (forM_, for_)
import Data.Foldable qualified as F
import Data.Functor (void, (<&>))
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Network.Mqtt
import Effectful.Reader.Static (Reader)
import FRP.Rhine
import GHC.Generics (Generic)
import Home.Reactive.MQTT
import Options.Applicative qualified as Opts
import Toml hiding (first, map)

class ToTopicFilter a where
  toTopicFilters :: a -> [TopicFilter]

data ESPresenseConfig = ESPresenseConfig
  { devices :: ![T.Text]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable ESPresenseConfig

instance ToTopicFilter ESPresenseConfig where
  toTopicFilters ESPresenseConfig {..} =
    [ "espresense" <> "devices" <> TopicFilter device <> wildOne
    | device <- devices
    ]

data SesameDevice = SesameDevice {uuid :: SesameUUID}
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable SesameDevice

data SesameConfig = SesameConfig
  { prefix :: !T.Text
  , devices :: !(HashMap T.Text SesameDevice)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec, HasItemCodec) via TomlTable SesameConfig

fromSesameConfig :: SesameConfig -> SesameEnv
fromSesameConfig SesameConfig {..} =
  SesameEnv
    { prefix = prefix
    , devices = devices
    , uuids = HM.fromList $ map (\(name, dev) -> (dev.uuid, (name, dev))) $ HM.toList devices
    }

newtype SesameUUID = UUID {raw :: T.Text}
  deriving newtype
    ( IsString
    , Show
    , Eq
    , Ord
    , HasItemCodec
    , HasCodec
    , Hashable
    )

data SesameEnv = SesameEnv
  { prefix :: !T.Text
  , devices :: !(HashMap T.Text SesameDevice)
  , uuids :: !(HashMap SesameUUID (T.Text, SesameDevice))
  }
  deriving (Show, Eq, Ord, Generic)

instance ToTopicFilter SesameConfig where
  toTopicFilters :: SesameConfig -> [TopicFilter]
  toTopicFilters SesameConfig {..}
    | null devices =
        [ TopicFilter prefix <> wildOne <> action
        | action <- ["set", "get"]
        ]
    | otherwise =
        [ TopicFilter prefix <> TopicFilter device.uuid.raw <> action
        | device <- F.toList devices
        , action <- ["set", "get"]
        ]

data Config = Config
  { host :: !T.Text
  , port :: !Int
  , user :: !(Maybe T.Text)
  , password :: !(Maybe T.Text)
  , espresense :: !(Maybe ESPresenseConfig)
  , sesame :: !(Maybe SesameConfig)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (HasCodec) via TomlTable Config

data CLIOpts = CLIOpts {configFile :: !FilePath}
  deriving (Show, Eq, Ord, Generic)

cliOptsP :: Opts.ParserInfo CLIOpts
cliOptsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc <> Opts.progDesc "Debug app" <> Opts.header "reactive-home"
  where
    p = do
      configFile <-
        Opts.strOption $
          Opts.long "config"
            <> Opts.short 'c'
            <> Opts.metavar "FILE"
            <> Opts.value "config.toml"
            <> Opts.showDefault
            <> Opts.help "Path to the configuration file (TOML format)"
      pure CLIOpts {..}

configCodec :: TomlCodec Config
configCodec = genericCodec

data LockStatus = LOCKED | UNLOCKED
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SesameStatus = SesamePayload
  { position :: !Int
  , lockCurrentState :: !LockStatus
  , batteryVoltage :: !Double
  , batteryLevel :: !Int
  , chargingState :: !T.Text
  , statusLowBattery :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ParseResult e a
  = ParseSuccess a
  | ParseFailure e
  | Skipped
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

data Action
  = LockSesame !T.Text
  | UnlockSesame !T.Text
  deriving (Show, Eq, Ord, Generic)

data HomeEnv = HomeEnv
  { mqtt :: {-# UNPACK #-} !MqttClient
  , sesame :: {-# UNPACK #-} !SesameEnv
  }
  deriving (Generic)

resultToEither :: ParseResult e a -> Either e (Maybe a)
resultToEither = \case
  ParseSuccess a -> Right (Just a)
  ParseFailure e -> Left e
  Skipped -> Right Nothing

data SesameError
  = InvalidMessage !MqttMessage
  | UnknownDevice !T.Text
  | InvalidPayload !String
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

parseSesameStatus ::
  SesameEnv ->
  MqttMessage ->
  ParseResult SesameError (T.Text, SesameDevice, SesameStatus)
parseSesameStatus env msg =
  case stripPrefix env.prefix msg.topic of
    Nothing -> Skipped
    Just (Topic rest)
      | (uuid, slashAction) <- T.breakOn "/" rest
      , let action = T.drop 1 slashAction
      , not (T.null action) ->
          case action of
            "get"
              | Just (name, device) <- HM.lookup (UUID uuid) env.uuids ->
                  case A.eitherDecode' $ LBS.fromStrict msg.payload of
                    Left err -> ParseFailure $ InvalidPayload err
                    Right status -> ParseSuccess (name, device, status)
              | otherwise -> ParseFailure (UnknownDevice uuid)
            _ -> Skipped
      | otherwise -> Skipped

sesameStatuses ::
  (Monad m) =>
  SesameEnv ->
  BehaviourF m t MqttMessage (ParseResult SesameError (t, T.Text, SesameDevice, SesameStatus))
sesameStatuses env = proc msg -> do
  TimeInfo {..} <- timeInfo -< ()
  returnA
    -<
      parseSesameStatus env msg
        <&> \(name, device, stat) -> (absolute, name, device, stat)

reportErrors ::
  (Exception e, MonadIO m) =>
  BehaviourF m t (ParseResult e a) (Maybe a)
reportErrors = arrM $ \case
  ParseFailure err -> do
    liftIO $ putStrLn $ "Error: " <> show err
    pure Nothing
  ParseSuccess a -> pure (Just a)
  Skipped -> pure Nothing

currentSesameStatus ::
  (MonadIO m) =>
  SesameEnv ->
  BehaviourF m t Message (HashMap T.Text SesameStatus)
currentSesameStatus env =
  feedback HM.empty $
    first
      (sesameStatuses env >-> reportErrors)
      >-> arr \(mmsg, prev) ->
        let new = case mmsg of
              Nothing -> prev
              Just (time, name, _, status) -> HM.insert name status prev
         in (new, new)

main :: IO ()
main = do
  CLIOpts {..} <- Opts.execParser cliOptsP
  config <- either (throwIO . userError . show) pure =<< decodeFileExact configCodec configFile
  print config
  let !topics =
        foldMap toTopicFilters config.espresense
          <> foldMap toTopicFilters config.sesame
  case NE.nonEmpty topics of
    Nothing -> putStrLn "No topics to subscribe to; exiting."
    Just ts -> do
      let !mqttCfg =
            MqttClockConfig
              { host = T.unpack $ host config
              , port = port config
              , user = user config
              , password = password config
              , clientId = "reactive-home-client"
              , subscriptions =
                  ts <&> \topic ->
                    Subscription
                      { topicFilter = topic
                      , retainHandling = SendOnSubscribe
                      , retainAsPublished = False
                      , noLocal = True
                      , qos = QoS1
                      }
              }
      let sesame = case config.sesame of
            Nothing -> pure ()
            Just sess ->
              (currentSesameStatus (fromSesameConfig sess) >-> arrMCl (liftIO . print))
      withMqttClient mqttCfg \client _ -> runEff $ runMqtt $ runConcurrent do
        flow $
          tagS
            >-> void
              ( arrMCl (liftIO . print @MqttMessage)
                  &&& sesame
              )
            @@ newMqttClock client
