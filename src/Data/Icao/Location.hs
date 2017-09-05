-- |
-- Provides data types and functions pertaining to locations
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Location
    ( AerodromeName
    , aerodromeParser
    , mkAerodromeName
    )
where

import           Text.ParserCombinators.Parsec

-- the name of an aerodrome, 4 uppercase characters.
newtype AerodromeName = AerodromeName String deriving (Show, Eq)

-- | 'Aerodrome' Parser.
aerodromeParser :: Parser AerodromeName
aerodromeParser =
    fmap AerodromeName (count 4 upper)

mkAerodromeName :: (Monad m) => String -> m AerodromeName
mkAerodromeName n
    | length n <= 0 = fail "empty Aerodrome Name"
    | length n > 4  = fail "max Aerodrome Name length (4) exceed"
    | otherwise     = return (AerodromeName n)
