-- |
-- Core data types used in various fields.
module Data.Icao.Lang
    ( FreeText
    , Natural2
    , Natural3
    , freeTextParser
    , natural2Parser
    , natural3Parser
    , mkFreeText
    , mkNatural2
    , mkNatural3
    ) where

import Data.Aeromess.Parser

-- | Free text, containing any character but '/', '-' and parentheses.
newtype FreeText =
    FreeText String
    deriving (Eq, Show)

-- | Natural integer in the range [0..99]
newtype Natural2 =
    Natural2 Int
    deriving (Eq, Show)

-- | Natural integer in the range [0..999]
newtype Natural3 =
    Natural3 Int
    deriving (Eq, Show)

-- | 'FreeText' parser
freeTextParser :: Parser FreeText
freeTextParser = fmap FreeText (some (noneOf "-/()"))

-- | 'Natural2' parser
natural2Parser :: Parser Natural2
natural2Parser = fmap Natural2 (natural 1 <|> natural 2)

-- | 'Natural3' parser
natural3Parser :: Parser Natural3
natural3Parser = fmap Natural3 (natural 1 <|> natural 2 <|> natural 3)

-- | 'FreeText' smart constructor. Fails if given string is not a valid free text.
mkFreeText :: (Monad m) => String -> m FreeText
mkFreeText s
    | null s = fail "empty free text"
    | not (any (\c -> c == '/' || c == '-' || c == '(' || c == ')') s) =
        fail ("invalid free text=" ++ s)
    | otherwise = return (FreeText s)

-- | 'Natural2' smart constructor. Fails if given integer is outside [0..99].
mkNatural2 :: (Monad m) => Int -> m Natural2
mkNatural2 n
    | n < 0 || n > 99 = fail ("invalid natural on 2 digits=" ++ show n)
    | otherwise = return (Natural2 n)

-- | 'Natural3' smart constructor. Fails if given integer is outside [0..999].
mkNatural3 :: (Monad m) => Int -> m Natural3
mkNatural3 n
    | n < 0 || n > 999 = fail ("invalid natural on 3 digits=" ++ show n)
    | otherwise = return (Natural3 n)
