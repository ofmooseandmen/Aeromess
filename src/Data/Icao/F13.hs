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
    a <- aerodromeParser
    t <- hhmmParser
    _ <- dash
    return (Data a t)

-- | ADEP parser.
adepParser :: Parser Aerodrome
adepParser = do
    a <- aerodromeParser
    _ <- dash
    return a
