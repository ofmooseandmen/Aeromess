-- |
-- ICAO Field Type 17 -  Arrival aerodrome and time.
-- Note: this is the terminal field of an ARR message
module Data.Icao.F17
    ( Data(adar, ata, adarName)
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Icao.Location
import Data.Icao.Time
import Prelude hiding (words)

-- | Field Type 17 data.
data Data = Data
    { adar :: Aerodrome
    , ata :: Hhmm
    , adarName :: Maybe String
    }

-- | Field Type 17 parser.
parser :: Parser Data
parser = do
    adar <- aerodromeParser
    ata <- hhmmParser
    adarName <- optional (space >> words)
    return (Data adar ata adarName)
