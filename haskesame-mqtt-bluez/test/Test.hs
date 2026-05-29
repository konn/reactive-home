module Main (main) where

import Network.Sesame.Mqtt.Bluez.App (configFileIn)

main :: IO ()
main = configFileIn "." `seq` putStrLn "Test suite not yet implemented"
