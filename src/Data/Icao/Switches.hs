{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper methods pertaining to ICAO Fields which are composed of switches.
module Data.Icao.Switches
    ( parser
    ) where

import Data.Aeromess.Parser

-- | Parses the switch indexed by a key associated with "Enum" 'a' and using the given
-- parser to parse the switch value. Returns 'Nothing' if string does not start with
-- given enum value.
parser
    :: forall a b c.
       (Bounded a, Enum a, Show a)
    => a -> Parser c -> (c -> b) -> Parser (Maybe b)
parser k p f = do
    pk <- key' show k
    if not (null pk)
        then do
            r <- p
            _ <- eos show (bounds :: [a])
            return (Just (f r))
        else return Nothing

eos
    :: forall a.
       (Bounded a, Enum a, Show a)
    => (a -> String) -> [a] -> Parser String
eos f b = try (optional space >> lookAhead (keys' f b <|> string "-" <|> string ")"))

key' :: (a -> String) -> a -> Parser String
key' f k = try (string (f k ++ "/"))

keys' :: (a -> String) -> [a] -> Parser String
keys' f v = choice (map (key' f) v)

bounds
    :: (Bounded a, Enum a)
    => [a]
bounds = [minBound .. maxBound]
