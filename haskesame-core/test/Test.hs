module Main (main) where

import Network.Sesame (ItemCode (Login), itemCodeToWord8)

main :: IO ()
main = itemCodeToWord8 Login `seq` putStrLn "Test suite not yet implemented"
