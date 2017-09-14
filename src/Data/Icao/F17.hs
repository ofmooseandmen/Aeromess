-- |
-- ICAO Field Type 17 -  Arrival aerodrome and time.
-- Note: this is the terminal field of an ARR message
module Data.Icao.F17
    ( Data(adar, ata, adarName)
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time

-- | Field Type 17 data.
data Data = Data
    { adar :: Aerodrome
    , ata :: Hhmm
    , adarName :: Maybe FreeText
    }

-- | Field Type 17 parser.
parser :: Parser Data
parser = do
    a <- aerodromeParser
    t <- hhmmParser
    n <- optional (space >> freeTextParser)
    return (Data a t n)
