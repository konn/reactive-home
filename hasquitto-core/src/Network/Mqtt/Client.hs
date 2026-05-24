{- | The high-level MQTT v5 client: connect to a broker, publish, subscribe, and
receive messages. Built on the pure codec ("Network.Mqtt.Codec") and the
pluggable connection abstraction ("Network.Mqtt.Connection").
-}
module Network.Mqtt.Client (
  -- * Configuration
  ConnectOptions (..),
  defaultConnectOptions,
  OverflowPolicy (..),
  TopicAliasMode (..),
  Authenticator (..),
  AuthChallenge (..),
  AuthResponse (..),
  PublishOptions (..),
  defaultPublishOptions,
  PublishResult (..),

  -- * Handle & session
  Client,
  Session (..),

  -- * Lifecycle
  connect,
  withClient,
  disconnect,
  disconnectWith,
  reconnect,
  waitClosed,
  isConnected,

  -- * Messaging
  publish,
  publish_,
  subscribe,
  subscribe1,
  unsubscribe,
  recvMessage,
  tryRecvMessage,
  recvMessageSTM,
  ping,
  reauthenticate,
) where

import Network.Mqtt.Client.Internal
