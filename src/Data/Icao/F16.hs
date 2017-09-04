-- |
-- ICAO Field Type 16 - Destination aerodrome and total estimated
-- elapsed time, destination alternate aerodrome(s).
module Data.Icao.F16
    ( Data(..)
    , adesParser
    , maybeAdesParser
    , parser
    )
where

import qualified Data.Icao.Time as T
import           Data.Icao.Util
import           Text.ParserCombinators.Parsec

data Data = Data
    { ades     :: String
    , totalEet :: T.Hhmm
    , altn1    :: Maybe String
    , altn2    :: Maybe String
    }

altn :: Parser (Maybe String)
altn =
    optionMaybe (char ' ' >> aerodromeParser)

adesParser :: Parser String
adesParser = do
    adep <- aerodromeParser
    satisfy (== '-')
    return adep

maybeAdesParser :: Parser (Maybe String)
maybeAdesParser =
    optionMaybe adesParser

parser :: Parser Data
parser = do
    ades  <- aerodromeParser
    tett  <- T.hhmmParser
    altn1 <- altn
    altn2 <- altn
    satisfy (== '-')
    return (Data ades tett altn1 altn2)
