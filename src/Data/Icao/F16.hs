-- |
-- ICAO Field Type 16 - Destination aerodrome and total estimated
-- elapsed time, destination alternate aerodrome(s).
module Data.Icao.F16
    ( Data(ades, totalEet, altn1, altn2)
    , adesParser
    , maybeAdesParser
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Icao.Location
import Data.Icao.Time

-- | Field Type 16 data.
data Data = Data
    { ades :: AerodromeName
    , totalEet :: Hhmm
    , altn1 :: Maybe AerodromeName
    , altn2 :: Maybe AerodromeName
    }

altn :: Parser (Maybe AerodromeName)
altn = optional (space >> aerodromeParser)

-- | ADES parser.
adesParser :: Parser AerodromeName
adesParser = do
    ades <- aerodromeParser
    dash
    return ades

-- | maybe ADES parser.
maybeAdesParser :: Parser (Maybe AerodromeName)
maybeAdesParser = optional (try adesParser)

-- | Field Type 16 'Data' parse.
parser :: Parser Data
parser = do
    ades <- aerodromeParser
    tett <- hhmmParser
    altn1 <- altn
    altn2 <- altn
    dash
    return (Data ades tett altn1 altn2)