{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Home.Reactive.MQTT where

import Control.Concurrent.STM qualified as IO
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Time (UTCTime, getCurrentTime)
import Data.Time qualified as IO
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static (unsafeEff_)
import GHC.Generics (Generic)
import Network.URI (URI (..), URIAuth (..), escapeURIString, isReserved, isUnescapedInURIComponent)

data MQTTSettings = MQTTSettings
  { hostname :: !String
  , port :: !Int
  , username :: !(Maybe String)
  , password :: !(Maybe String)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)
