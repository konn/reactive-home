module Main (main) where

import SimpleBLE (bluetoothEnabled)

main :: IO ()
main = do
  _ <- bluetoothEnabled
  putStrLn "Test suite not yet implemented"
