module Main (main) where

import Control.Applicative ((<**>))
import Network.Sesame.Mqtt.SimpleBLE.App (loadConfig, runApp)
import Options.Applicative qualified as Opts

newtype CLIOpts = CLIOpts {configFile :: FilePath}

cliOptsP :: Opts.ParserInfo CLIOpts
cliOptsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc
      <> Opts.progDesc "Bridge Sesame 5 BLE devices to MQTT using SimpleBLE"
      <> Opts.header "haskesame-mqtt-simpleble"
  where
    p =
      CLIOpts
        <$> Opts.strOption
          ( Opts.long "config"
              <> Opts.short 'c'
              <> Opts.metavar "FILE"
              <> Opts.value "config.toml"
              <> Opts.showDefault
              <> Opts.help "TOML configuration file"
          )

main :: IO ()
main = do
  CLIOpts {configFile} <- Opts.execParser cliOptsP
  loadConfig configFile >>= runApp
