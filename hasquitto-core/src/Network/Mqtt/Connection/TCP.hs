{- | A plain-TCP backend for the MQTT 'Conn' abstraction, built directly on the
@network@ package (in the spirit of streaming-commons' @runTCPClient@, without
depending on it). TLS is a separate backend that produces the same 'Conn'.
-}
module Network.Mqtt.Connection.TCP (
  ClientSettings (..),
  clientSettings,
  tcpConnection,
  runTCPClient,
) where

import Control.Exception (bracket, onException)
import Network.Mqtt.Connection.Internal (Conn (..), Connection (..), makeConnection)
import Network.Socket (
  AddrInfo (..),
  HostName,
  PortNumber,
  SocketOption (NoDelay),
  SocketType (Stream),
  close,
  connect,
  defaultHints,
  getAddrInfo,
  openSocket,
  setSocketOption,
  withSocketsDo,
 )
import Network.Socket.ByteString (recv, sendAll)

-- | Where to connect a TCP client.
data ClientSettings = ClientSettings
  { host :: !HostName
  , port :: !PortNumber
  }
  deriving stock (Show, Eq)

-- | Build 'ClientSettings' from a host and port.
clientSettings :: HostName -> PortNumber -> ClientSettings
clientSettings = ClientSettings

{- | Resolve, connect, and wrap a TCP socket as a buffered 'Conn'. The caller is
responsible for closing it (e.g. via 'runTCPClient' or the client's lifecycle).
-}
tcpConnection :: ClientSettings -> IO Conn
tcpConnection cs = withSocketsDo do
  addr <- resolve cs
  sock <- openSocket addr
  ( do
      connect sock (addrAddress addr)
      setSocketOption sock NoDelay 1
    )
    `onException` close sock
  makeConnection (recv sock 16384) (sendAll sock) (close sock)

{- | Run an action with a freshly-connected TCP 'Conn', closing it afterwards
(even on exception).
-}
runTCPClient :: ClientSettings -> (Conn -> IO a) -> IO a
runTCPClient cs = bracket (tcpConnection cs) (\c -> c.base.connectionClose)

resolve :: ClientSettings -> IO AddrInfo
resolve cs = do
  let hints = defaultHints {addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just cs.host) (Just (show cs.port))
  case addrs of
    (a : _) -> pure a
    [] -> ioError (userError ("could not resolve host: " <> cs.host))
