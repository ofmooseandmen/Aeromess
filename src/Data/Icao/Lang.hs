-- |
-- Core data types used in various fields.
module Data.Icao.Lang
    ( FreeText
    , freeTextParser
    , mkFreeText
    ) where

import Data.Aeromess.Parser

-- | Free text, containing any character but '/', '-' and parentheses.
newtype FreeText =
    FreeText String
    deriving (Eq, Show)

-- | 'FreeText' parser
freeTextParser :: Parser FreeText
freeTextParser = fmap FreeText (some (noneOf "-/()"))

-- | 'FreeText' smart constructor. Fails if given string is not a valid free text.
mkFreeText :: (Monad m) => String -> m FreeText
mkFreeText s
    | null s = fail "empty free text"
    | any (\c -> c == '/' || c == '-' || c == '(' || c == ')') s = fail ("invalid free text=" ++ s)
    | otherwise = return (FreeText s)
