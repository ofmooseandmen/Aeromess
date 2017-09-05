-- |
-- ICAO Field Type 13 - Departure aerodrome and time.
module Data.Icao.F13
    ( Data (adep, time)
    , adepParser
    , parser
    )
where

import           Data.Icao.Location
import           Data.Icao.Time
import           Text.ParserCombinators.Parsec

-- | Field Type 13 data.
data Data = Data
    { adep :: AerodromeName
    , time :: Hhmm
    }

-- | Field Type 13 'Data' parser.
parser :: Parser Data
parser = do
    adep <- aerodromeParser
    time <- hhmmParser
    satisfy (== '-')
    return (Data adep time)

-- | ADEP parser.
adepParser :: Parser AerodromeName
adepParser = do
    adep <- aerodromeParser
    satisfy (== '-')
    return adep
