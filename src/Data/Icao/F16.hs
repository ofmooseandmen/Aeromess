-- |
-- ICAO Field Type 16 - Destination aerodrome and total estimated
-- elapsed time, destination alternate aerodrome(s).
module Data.Icao.F16
    ( Data(ades, totalEet, altn1, altn2)
    , adesParser
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Icao.Location
import Data.Icao.Time

-- | Field Type 16 data.
data Data = Data
    { ades :: Aerodrome
    , totalEet :: Hhmm
    , altn1 :: Maybe Aerodrome
    , altn2 :: Maybe Aerodrome
    }

altn :: Parser (Maybe Aerodrome)
altn = optional (space >> aerodromeParser)

-- | ADES parser.
adesParser :: Parser Aerodrome
adesParser = do
    a <- aerodromeParser
    _ <- dash
    return a

-- | Field Type 16 parser.
parser :: Parser Data
parser = do
    a <- aerodromeParser
    tett <- hhmmParser
    a1 <- altn
    a2 <- altn
    _ <- dash
    return (Data a tett a1 a2)
