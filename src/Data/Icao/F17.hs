-- |
-- ICAO Field Type 17 -  Arrival aerodrome and time.
-- Note: this is the terminal field of an ARR message
module Data.Icao.F17
    ( Data (..)
    , parser
    )
where

import qualified Data.Icao.Time as T
import           Data.Icao.Util
import           Text.ParserCombinators.Parsec

data Data = Data
    { adar     :: String
    , ata      :: T.Hhmm
    , adarName :: Maybe String
    }

parser :: Parser Data
parser = do
    adar <- aerodromeParser
    ata  <- T.hhmmParser
    adarName <- optionMaybe aerodromeParser
    satisfy (== ')')
    return (Data adar ata adarName)
