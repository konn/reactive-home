module Main (main) where

import Network.Sesame.Mqtt.Bluez.App (configCodec)

main :: IO ()
main = configCodec `seq` putStrLn "Test suite not yet implemented"
