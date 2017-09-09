{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

module Data.Icao.Switches
   ( parser
   ) where

import Data.Aeromess.Parser

parser :: forall a b c . (Bounded a, Enum a, Show a) => a -> Parser c -> (c -> b) -> Parser b
parser k p f = do
    key' (show :: (a -> String)) k
    r <- p
    eos (show :: (a -> String)) (bounds :: [a])
    return (f r)

eos :: forall a . (Bounded a, Enum a, Show a) => (a -> String) -> [a] -> Parser String
eos f b = try (optional space >> lookAhead ((keys' f b) <|> string "-" <|> string ")"))

key' :: (a -> String) -> a -> Parser String
key' f k = string (f k ++ "/")

keys' :: (a -> String) -> [a] -> Parser String
keys' f v = choice (map (\k -> key' f k) v)

bounds :: (Bounded a, Enum a) => [a]
bounds = [minBound .. maxBound]
