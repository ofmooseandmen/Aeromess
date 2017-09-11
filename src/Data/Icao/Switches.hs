{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
-- | Helper methods pertaining to ICAO Fields which are composed of switches.
module Data.Icao.Switches
   ( parser
   ) where

import Data.Aeromess.Parser

-- | Parses the switch indexed by a key associated with "Enum" 'a' and using the given
-- parser to parse the switch value.
parser :: forall a b c . (Bounded a, Enum a, Show a) => a -> Parser c -> (c -> b) -> Parser b
parser k p f = do
    key' (show :: (a -> String)) k
    r <- p
    eos (show :: (a -> String)) (bounds :: [a])
    return (f r)

eos :: forall a . (Bounded a, Enum a, Show a) => (a -> String) -> [a] -> Parser String
eos f b = try (optional space >> lookAhead (keys' f b <|> string "-" <|> string ")"))

key' :: (a -> String) -> a -> Parser String
key' f k = string (f k ++ "/")

keys' :: (a -> String) -> [a] -> Parser String
keys' f v = choice (map (key' f) v)

bounds :: (Bounded a, Enum a) => [a]
bounds = [minBound .. maxBound]
