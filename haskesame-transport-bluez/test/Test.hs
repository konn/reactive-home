module Main (main) where

import Network.Sesame.Transport.Bluez (defaultBluezConfig)

main :: IO ()
main = defaultBluezConfig `seq` putStrLn "Test suite not yet implemented"
