-- |
-- Provides data types and functions pertaining to time, duration and dates
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Time
    ( Hhmm (hour, minute)
    , Date (year, month, day)
    , hhmmParser
    , mkHhmm
    )
where

import           Text.ParserCombinators.Parsec

-- |'Hhmm' represents a time or duration expressed with hours and minutes only.
data Hhmm = Hhmm { hour   :: Int
                 , minute :: Int
                 } deriving (Show, Eq)

-- |'Date' represents a date expressed zith year, month and day.
data Date = Date { year  :: Int
                 , month :: Int
                 , day   :: Int
                 } deriving (Show, Eq)

numbers :: Int -> Parser Int
numbers digits =
       fmap read (count digits digit)

-- | 'Hhmm' parser.
hhmmParser :: Parser Hhmm
hhmmParser = do
    hour <- numbers 2;
    minute <- numbers 2;
    if hour > 23 then
        unexpected ("hour=" ++ show hour)
    else if minute > 59 then
        unexpected ("minute=" ++ show minute)
    else
        return (Hhmm hour minute)

mkHhmm :: (Monad m) => Int -> Int -> m Hhmm
mkHhmm hh mm
    | hh < 0 || hh > 23 = fail "invalid hour"
    | mm < 0 || mm > 59 = fail "invalid minute"
    | otherwise         = return (Hhmm hh mm)
