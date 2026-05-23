{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Home.Reactive.Orphans () where

import Control.Monad
import Control.Monad.Schedule.Class
import Data.List.NonEmpty (NonEmpty (..))
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.MVar

instance (Concurrent :> es) => MonadSchedule (Eff es) where
  schedule as = do
    var <- newEmptyMVar
    forM_ as $ \action -> forkIO $ putMVar var =<< action
    a <- takeMVar var
    as' <- drain var
    let remaining = replicate (length as - 1 - length as') $ takeMVar var
    return (a :| as', remaining)
    where
      drain :: MVar a -> Eff es [a]
      drain var = do
        aMaybe <- tryTakeMVar var
        case aMaybe of
          Just a -> do
            as' <- drain var
            return $ a : as'
          Nothing -> return []

--
