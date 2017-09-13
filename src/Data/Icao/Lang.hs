-- |
-- Core data types used in various fields.
module Data.Icao.Lang
    ( FreeText
    , Natural2
    , Natural3
    , Natural4
    , freeTextParser
    , natural2Parser
    , natural3Parser
    , natural4Parser
    , mkFreeText
    , mkNatural2
    , mkNatural3
    , mkNatural4
    ) where

import Data.Aeromess.Parser

-- | Free text, containing any character but '/', '-' and parentheses.
newtype FreeText =
    FreeText String
    deriving (Eq, Show)

-- | Natural integer in the range [0 .. 99].
newtype Natural2 =
    Natural2 Int
    deriving (Eq, Show)

-- | Natural integer in the range [0 .. 999].
newtype Natural3 =
    Natural3 Int
    deriving (Eq, Show)

-- | Natural integer in the range [0 .. 9999].
newtype Natural4 =
    Natural4 Int
    deriving (Eq, Show)

-- | 'FreeText' parser
freeTextParser :: Parser FreeText
freeTextParser = fmap FreeText (some (noneOf "-/()"))

naturalParser :: Int -> (Int -> a) -> Parser a
naturalParser d f = fmap f (choice [try (natural x) | x <- [d,d - 1 .. 0]])

-- | 'Natural2' parser
natural2Parser :: Parser Natural2
natural2Parser = naturalParser 2 Natural2

-- | 'Natural3' parser
natural3Parser :: Parser Natural3
natural3Parser = naturalParser 3 Natural3

-- | 'Natural4' parser
natural4Parser :: Parser Natural4
natural4Parser = naturalParser 4 Natural4

-- | 'FreeText' smart constructor. Fails if given string is not a valid free text.
mkFreeText :: (Monad m) => String -> m FreeText
mkFreeText s
    | null s = fail "empty free text"
    | not (any (\c -> c == '/' || c == '-' || c == '(' || c == ')') s) =
        fail ("invalid free text=" ++ s)
    | otherwise = return (FreeText s)

-- | 'Natural2' smart constructor. Fails if given integer is outside [0 .. 99].
mkNatural2 :: (Monad m) => Int -> m Natural2
mkNatural2 n
    | n < 0 || n > 99 = fail ("invalid natural on 2 digits=" ++ show n)
    | otherwise = return (Natural2 n)

-- | 'Natural3' smart constructor. Fails if given integer is outside [0 .. 999].
mkNatural3 :: (Monad m) => Int -> m Natural3
mkNatural3 n
    | n < 0 || n > 999 = fail ("invalid natural on 3 digits=" ++ show n)
    | otherwise = return (Natural3 n)

-- | 'Natural4' smart constructor. Fails if given integer is outside [0 .. 9999].
mkNatural4 :: (Monad m) => Int -> m Natural4
mkNatural4 n
    | n < 0 || n > 9999 = fail ("invalid natural on 4 digits=" ++ show n)
    | otherwise = return (Natural4 n)
