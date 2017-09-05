-- |
-- ICAO Field Type 17 -  Arrival aerodrome and time.
-- Note: this is the terminal field of an ARR message
module Data.Icao.F17
    ( Data (adar, ata, adarName)
    , parser
    )
where

import           Data.Icao.Location
import           Data.Icao.Time
import           Text.ParserCombinators.Parsec

-- | Field Type 17 data.
data Data = Data
    { adar     :: AerodromeName
    , ata      :: Hhmm
    , adarName :: Maybe String
    }

-- | Field Type 17 'Data' parser.
parser :: Parser Data
parser = do
    adar <- aerodromeParser
    ata  <- hhmmParser
    adarName <- optionMaybe (space >> many1 (choice [alphaNum, space]))
    return (Data adar ata adarName)
