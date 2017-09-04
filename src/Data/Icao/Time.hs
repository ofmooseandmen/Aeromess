-- |
-- Provides data types and functions pertaining to time, duration and dates
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Time
    ( Hhmm(..)
    , Date
    , hhmmParser
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

-- | 'Hhmm' parser
hhmmParser :: Parser Hhmm
hhmmParser = do
    hour <- numbers 2;
    minute <- numbers 2;
    if hour > 23 then
        unexpected "Invalid hour"
    else if minute > 59 then
        unexpected "Invalid minute"
    else
        return (Hhmm hour minute)
