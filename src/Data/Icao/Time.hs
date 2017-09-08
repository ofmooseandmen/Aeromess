-- |
-- Provides data types and functions pertaining to time, duration and dates
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Time
    ( Hhmm(hour, minute)
    , Date(year, month, day)
    , hhmmParser
    , mkHhmm
    ) where

import Data.Aeromess.Parser

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
    hour <- positive 2
    minute <- positive 2
    mkHhmm hour minute

mkHhmm :: (Monad m) => Int -> Int -> m Hhmm
mkHhmm hh mm
    | hh < 0 || hh > 23 = fail ("invalid hour=" ++ show hh)
    | mm < 0 || mm > 59 = fail ("invalid minute=" ++ show mm)
    | otherwise = return (Hhmm hh mm)
