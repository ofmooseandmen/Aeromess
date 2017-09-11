-- |
-- Provides data types and functions pertaining to time, duration and dates
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Time
    ( Hhmm(hour, minute)
    , Date(year, month, day)
    , Parser
    , Error(message, column)
    , hhmmParser
    , dateParser
    , mkHhmm
    , mkDate
    , parseHhmm
    , parseDate
    ) where

import Data.Aeromess.Parser
import Data.Either

-- |'Hhmm' represents a time or duration expressed with hours and minutes only.
data Hhmm = Hhmm
    { hour :: Int
    , minute :: Int
    } deriving (Eq, Show)

-- |'Date' represents a date expressed zith year, month and day.
data Date = Date
    { year :: Int
    , month :: Int
    , day :: Int
    } deriving (Eq, Show)

-- | 'Hhmm' parser.
hhmmParser :: Parser Hhmm
hhmmParser = do
    hh <- positive 2
    mm <- positive 2
    mkHhmm hh mm

-- | 'Date' parser.
dateParser :: Parser Date
dateParser = do
    yy <- positive 2
    mm <- positive 2
    dd <- positive 2
    mkDate yy mm dd

-- | Parses the given textual representation of a 'Hhmm'.
-- return either an 'Error' ('Left') or the parsed 'Hhmm' ('Right').
parseHhmm :: String -> Either Error Hhmm
parseHhmm = runParser hhmmParser

-- | Parses the given textual representation of a 'Date'.
-- return either an 'Error' ('Left') or the parsed 'Date' ('Right').
parseDate :: String -> Either Error Date
parseDate = runParser dateParser

-- | 'Hhmm' smart constructor. Fails if given hour and/or minute are not valid.
mkHhmm :: (Monad m) => Int -> Int -> m Hhmm
mkHhmm hh mm
    | hh < 0 || hh > 23 = fail ("invalid hour=" ++ show hh)
    | mm < 0 || mm > 59 = fail ("invalid minute=" ++ show mm)
    | otherwise = return (Hhmm hh mm)

-- | 'Date' smart constructor. Fails if given year and/or month and/or day are not valid.
mkDate yy mm dd
    | yy < 0 || yy > 99 = fail ("invalid year=" ++ show yy)
    | mm < 1 || mm > 12 = fail ("invalid month=" ++ show mm)
    | dd < 1 || dd > 31 = fail ("invalid day=" ++ show dd)
    | otherwise = return (Date yy mm dd)
