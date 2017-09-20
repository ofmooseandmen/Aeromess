-- |
-- Provides data types and functions pertaining to time, duration and dates
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Time
    (
    -- * Data
      Hhmm(hour, minute)
    , DayTime(dayOfMonth, time)
    , Date(year, month, day)
    -- * Parsers
    , hhmmParser
    , dateParser
    , dayTimeParser
    , parseHhmm
    , parseDate
    , parseDayTime
    -- * Smart constructors
    , mkHhmm
    , mkDate
    , mkDayTime
    ) where

import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Aeromess.Parser
import Data.Either ()

-- |'Hhmm' represents a time or duration expressed with hours and minutes only
-- in the range 0000 .. 2359.
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

-- | day and time.
data DayTime = DayTime
    { dayOfMonth :: Int -- ^ day of month
    , time :: Hhmm -- ^ hour minute
    } deriving (Eq, Show)

-- | 'Hhmm' parser.
hhmmParser :: Parser Hhmm
hhmmParser = do
    hh <- natural 2
    mm <- natural 2
    mkHhmm hh mm

-- | 'Date' parser.
dateParser :: Parser Date
dateParser = do
    yy <- natural 2
    mm <- natural 2
    dd <- natural 2
    mkDate yy mm dd

-- | 'DayTime' parser.
dayTimeParser :: Parser DayTime
dayTimeParser = do
    dd <- natural 2
    hhmm <- hhmmParser
    mkDayTime dd (hour hhmm) (minute hhmm)

-- | Parses the given textual representation of a 'Hhmm'.
-- return either an error message ('Left') or the parsed 'Hhmm' ('Right').
parseHhmm :: String -> Either String Hhmm
parseHhmm = runParser hhmmParser

-- | Parses the given textual representation of a 'Date'.
-- return either an error message ('Left') or the parsed 'Date' ('Right').
parseDate :: String -> Either String Date
parseDate = runParser dateParser

-- | Parses the given textual representation of a 'DayTime'.
-- return either an error message ('Left') or the parsed 'DayTime' ('Right').
parseDayTime :: String -> Either String DayTime
parseDayTime = runParser dayTimeParser

-- | 'Hhmm' smart constructor. Fails if given hour and/or minute are not valid.
mkHhmm
    :: (MonadFail m)
    => Int -> Int -> m Hhmm
mkHhmm hh mm
    | hh < 0 || hh > 23 = fail ("invalid hour=" ++ show hh)
    | mm < 0 || mm > 59 = fail ("invalid minute=" ++ show mm)
    | otherwise = return (Hhmm hh mm)

-- | 'Date' smart constructor. Fails if given year and/or month and/or day are not valid.
mkDate
    :: (MonadFail m)
    => Int -> Int -> Int -> m Date
mkDate yy mm dd
    | yy < 0 || yy > 99 = fail ("invalid year=" ++ show yy)
    | mm < 1 || mm > 12 = fail ("invalid month=" ++ show mm)
    | dd < 1 || dd > 31 = fail ("invalid day=" ++ show dd)
    | otherwise = return (Date yy mm dd)

-- | 'DayTime' smart constructor. Fails if given day and/or hour and/or minute are not valid.
mkDayTime
    :: (MonadFail m)
    => Int -> Int -> Int -> m DayTime
mkDayTime dd hh mm
    | dd < 1 || dd > 31 = fail ("invalid day=" ++ show dd)
    | otherwise = fmap (DayTime dd) (mkHhmm hh mm)
