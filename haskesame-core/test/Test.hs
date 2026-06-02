module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Network.Sesame.Codec
import Network.Sesame.Types

main :: IO ()
main = do
  assertEqual "lock command item and history tag" (BS.pack [82, 8, 115, 115, 109, 50, 109, 113, 116, 116]) (encodedCommand Lock "ssm2mqtt")
  assertEqual "unlock command item and history tag" (BS.pack [83, 8, 115, 115, 109, 50, 109, 113, 116, 116]) (encodedCommand Unlock "ssm2mqtt")
  assertEqual "mech setting command is six-byte payload" (BS.pack [80, 0x34, 0x12, 0x02, 0xff, 0x2c, 0x01]) encodedMechSettingCommand
  assertEqual "decode mech setting roundtrip" (Right mechSetting) (decodeSesame5MechSetting (encodeSesame5MechSetting mechSetting))
  assertEqual "mech status item is publish item code 81" 81 (itemCodeToWord8 MechStatus)
  putStrLn "haskesame-core protocol tests passed"

encodedCommand :: ItemCode -> Text -> ByteString
encodedCommand item name =
  encodeCommand (SesameCommand item (createHistoryTag name).unHistoryTag)

encodedMechSettingCommand :: ByteString
encodedMechSettingCommand =
  encodeCommand (SesameCommand MechSetting (encodeSesame5MechSetting mechSetting))

mechSetting :: Sesame5MechSetting
mechSetting =
  Sesame5MechSetting
    { lockPosition = 0x1234
    , unlockPosition = -254
    , autoLockDuration = 300
    }

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) do
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)
