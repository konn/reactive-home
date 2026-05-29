module Main (main) where

import Control.Applicative ((<**>))
import Network.Sesame.Mqtt.Bluez.App (runApp)
import Options.Applicative qualified as Opts

newtype CLIOpts = CLIOpts {configDir :: FilePath}

cliOptsP :: Opts.ParserInfo CLIOpts
cliOptsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc
      <> Opts.progDesc "Bridge Sesame 5 BLE devices to MQTT using BlueZ"
      <> Opts.header "haskesame-mqtt-bluez"
  where
    p =
      CLIOpts
        <$> Opts.strOption
          ( Opts.long "config-dir"
              <> Opts.short 'c'
              <> Opts.metavar "DIR"
              <> Opts.value "."
              <> Opts.showDefault
              <> Opts.help "Directory containing config.toml"
          )

main :: IO ()
main = do
  CLIOpts {configDir} <- Opts.execParser cliOptsP
  runApp configDir
