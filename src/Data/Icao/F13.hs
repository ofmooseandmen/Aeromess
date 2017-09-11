-- |
-- ICAO Field Type 13 - Departure aerodrome and time.
module Data.Icao.F13
    ( Data(adep, time)
    , adepParser
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Icao.Location
import Data.Icao.Time

-- | Field Type 13 data.
data Data = Data
    { adep :: Aerodrome
    , time :: Hhmm
    }

-- | Field Type 13 parser.
parser :: Parser Data
parser = do
    adep <- aerodromeParser
    time <- hhmmParser
    dash
    return (Data adep time)

-- | ADEP parser.
adepParser :: Parser Aerodrome
adepParser = do
    adep <- aerodromeParser
    dash
    return adep
