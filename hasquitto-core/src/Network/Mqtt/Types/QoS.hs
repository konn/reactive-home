-- | Quality of Service levels, the MQTT delivery-guarantee vocabulary.
module Network.Mqtt.Types.QoS (
  QoS (..),
) where

{- | MQTT Quality of Service level.

The 'Enum' instance matches the 2-bit wire encoding: @'fromEnum' 'QoS0' == 0@,
@'QoS1' == 1@, @'QoS2' == 2@. The wire value @3@ (@0b11@) is malformed and has
no corresponding constructor.
-}
data QoS
  = -- | At most once delivery.
    QoS0
  | -- | At least once delivery.
    QoS1
  | -- | Exactly once delivery.
    QoS2
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)
