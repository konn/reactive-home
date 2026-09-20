{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Network.SwitchBot.SimpleBLE (getScanner, scanReadings)
import System.IO (hPutStrLn, stderr)

-- | One discovery scan, printing supported devices as JSON lines.
main :: IO ()
main = do
  adapter <- getScanner Nothing
  readings <- scanReadings adapter 20000 (hPutStrLn stderr)
  mapM_ (LBS.putStrLn . encode) readings
