module Main (main) where

import Network.Sesame.Transport.SimpleBLE (defaultSimpleBLEConfig)

main :: IO ()
main = defaultSimpleBLEConfig "00:00:00:00:00:00" `seq` putStrLn "Test suite not yet implemented"
