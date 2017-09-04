-- |
-- ICAO Field Type 13
module Data.Icao.F13
    ( Data(..)
    , parser
    )

where

import qualified Data.Icao.Time as T
import           Data.Icao.Util
import           Text.ParserCombinators.Parsec

data Data = Data
    { adep :: String
    , time :: T.Hhmm
    }

parser :: Parser Data
parser = do
    adep <- aerodromeParser
    time <- T.hhmmParser
    satisfy (== '-')
    return (Data adep time)

adepParser :: Parser String
adepParser = do
    adep <- aerodromeParser
    satisfy (== '-')
    return adep
