{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Network.SwitchBot.Bluez (scanSensors)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = scanSensors Nothing 20000 (hPutStrLn stderr) >>= mapM_ (LBS.putStrLn . encode)
