{-# LANGUAGE DerivingStrategies #-}

module Home.Reactive.App.Types (
  ParseResult (..),
) where

import GHC.Generics (Generic)

data ParseResult e a
  = ParseSuccess a
  | ParseFailure e
  | Skipped
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
