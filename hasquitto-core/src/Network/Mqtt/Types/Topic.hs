-- | Topic names (for publishing) and topic filters (for subscribing).
module Network.Mqtt.Types.Topic (
  -- * Types
  Topic (..),
  TopicFilter (..),
  TopicError (..),

  -- * Smart constructors
  mkTopic,
  mkTopicFilter,

  -- * Levels & matching
  topicLevels,
  filterLevels,
  matches,
) where

import Data.Text (Text)
import Data.Text qualified as T

{- | A topic /name/, used when publishing. A valid topic name is non-empty,
contains no NUL character, contains no wildcard characters (@+@, @#@), and is at
most 65535 UTF-8 bytes. Construct with 'mkTopic'.
-}
newtype Topic = Topic {raw :: Text}
  deriving stock (Show, Eq, Ord)

{- | A topic /filter/, used when subscribing. May contain the wildcards @+@
(single level) and @#@ (multi level, final level only). Construct with
'mkTopicFilter'.
-}
newtype TopicFilter = TopicFilter {raw :: Text}
  deriving stock (Show, Eq, Ord)

-- | Why a 'Text' is not a valid topic name or filter.
data TopicError
  = -- | The string was empty.
    EmptyTopic
  | -- | The string contained a NUL (@U+0000@) character.
    ContainsNul
  | -- | A topic /name/ contained a wildcard character.
    ContainsWildcard
  | {- | A topic /filter/ used a wildcard illegally (not occupying a whole level,
    or @#@ not in final position).
    -}
    BadWildcard Text
  | -- | The UTF-8 encoding exceeded 65535 bytes.
    TooLong
  deriving stock (Show, Eq)

maxTopicBytes :: Int
maxTopicBytes = 65535

utf8Length :: Text -> Int
utf8Length = T.foldl' step 0
  where
    step !n c
      | o < 0x80 = n + 1
      | o < 0x800 = n + 2
      | o < 0x10000 = n + 3
      | otherwise = n + 4
      where
        o = fromEnum c

-- | Validate and wrap a topic name. See t'Topic' for the rules.
mkTopic :: Text -> Either TopicError Topic
mkTopic t
  | T.null t = Left EmptyTopic
  | T.any (== '\0') t = Left ContainsNul
  | T.any (\c -> c == '+' || c == '#') t = Left ContainsWildcard
  | utf8Length t > maxTopicBytes = Left TooLong
  | otherwise = Right (Topic t)

-- | Validate and wrap a topic filter. See t'TopicFilter' for the rules.
mkTopicFilter :: Text -> Either TopicError TopicFilter
mkTopicFilter t
  | T.null t = Left EmptyTopic
  | T.any (== '\0') t = Left ContainsNul
  | utf8Length t > maxTopicBytes = Left TooLong
  | not (validWildcards levels) = Left (BadWildcard t)
  | otherwise = Right (TopicFilter t)
  where
    levels = T.splitOn "/" t

{- | Each level must either be a clean wildcard occupying the whole level, or
contain no wildcard at all; @#@ must be the last level.
-}
validWildcards :: [Text] -> Bool
validWildcards [] = True
validWildcards (lvl : rest)
  | lvl == "#" = null rest
  | lvl == "+" = validWildcards rest
  | T.any (\c -> c == '+' || c == '#') lvl = False
  | otherwise = validWildcards rest

-- | Split a topic name into its levels (slash-separated).
topicLevels :: Topic -> [Text]
topicLevels (Topic t) = T.splitOn "/" t

-- | Split a topic filter into its levels (slash-separated).
filterLevels :: TopicFilter -> [Text]
filterLevels (TopicFilter t) = T.splitOn "/" t

{- | Does a topic filter match a topic name, per the MQTT v5 matching rules
(§4.7)?

Wildcards do not match topic names whose first level begins with @\'$\'@ (e.g.
@$SYS/...@).

>>> let Right f = mkTopicFilter "sport/+/player1"
>>> let Right t = mkTopic "sport/tennis/player1"
>>> matches f t
True
-}
matches :: TopicFilter -> Topic -> Bool
matches (TopicFilter f) (Topic t) =
  case (fls, tls) of
    (w : _, d : _) | isWild w && startsWithDollar d -> False
    _ -> go fls tls
  where
    fls = T.splitOn "/" f
    tls = T.splitOn "/" t
    isWild w = w == "+" || w == "#"
    startsWithDollar d = case T.uncons d of
      Just ('$', _) -> True
      _ -> False
    go ("#" : _) _ = True
    go ("+" : fs) (_ : ts) = go fs ts
    go (x : fs) (y : ts) = x == y && go fs ts
    go [] [] = True
    go _ _ = False
