module Main (main) where

import Network.Sesame.Mqtt (defaultBridgeConfig)

main :: IO ()
main = defaultBridgeConfig `seq` putStrLn "Test suite not yet implemented"
