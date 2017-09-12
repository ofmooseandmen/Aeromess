-- |
-- Provides data types and functions pertaining to locations
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Location
    ( Aerodrome
    , SignificantPoint(CodedDesignator, Position, BearingDistance,
                 latitude, longitude, reference, bearing, distance)
    , aerodromeParser
    , significantPointParser
    , parseAerodrome
    , parseSignificantPoint
    , mkAerodrome
    , mkCodedDesignator
    , mkPosition
    , mkBearingDistance
    ) where

import Data.Aeromess.Parser
import Data.Maybe
import Data.Char
import Data.Either()

-- | the name of an aerodrome, 4 uppercase characters.
newtype Aerodrome =
    Aerodrome String
    deriving (Eq, Show)

-- | a significant point
data SignificantPoint
    -- | Point coded designator (e.g. 'RASMU'), 2 to 5 characters.
    = CodedDesignator String
    -- | Point Position; latitude and longitude in decimal degrees.
    | Position { latitude :: Float
               , longitude :: Float }
    -- | Bearing and distance from a reference point, bearing in degrees, distance in nautical miles.
    | BearingDistance { reference :: String
                      , bearing :: Int
                      , distance :: Int }
    deriving (Eq, Show)

-- | 'Aerodrome' Parser.
aerodromeParser :: Parser Aerodrome
aerodromeParser = do
    a <- word
    mkAerodrome a

-- | Parses the given textual representation of an 'Aerodrome'.
-- return either an 'Error' ('Left') or the parsed 'Aerodrome' ('Right').
parseAerodrome :: String -> Either Error Aerodrome
parseAerodrome = runParser aerodromeParser

-- | 'Aerodrome' smart constructor. Fails if the given name is not a valid.
mkAerodrome :: (Monad m) => String -> m Aerodrome
mkAerodrome n
    | length n /= 4 || not (all isUpper n) =
        fail ("invalid aerodrome name=" ++ n ++ " expected 4 [A-Z] characters")
    | otherwise = return (Aerodrome n)

-- | 'SignificantPoint' Parser.
significantPointParser :: Parser SignificantPoint
significantPointParser = namedPointParser <|> latLongParser

-- | Parses the given textual representation of a 'SignificantPoint'.
-- return either an 'Error' ('Left') or the parsed 'SignificantPoint' ('Right').
parseSignificantPoint :: String -> Either Error SignificantPoint
parseSignificantPoint = runParser significantPointParser

-- | 'CodedDesignator' 'SignificantPoint' smart constructor. Fails if the given name
-- is not a valid.
mkCodedDesignator :: (Monad m) => String -> m SignificantPoint
mkCodedDesignator n
    | length n < 2 || length n > 5 =
        fail ("invalid coded designator=" ++ n ++ " expected 2 to 5 [A-Z] characters")
    | otherwise = return (CodedDesignator n)

-- | 'BearingDistance' 'SignificantPoint' smart constructor. Fails if the given name
-- and/or bearing and/or distance are not a valid.
mkBearingDistance :: (Monad m) => String -> Int -> Int -> m SignificantPoint
mkBearingDistance n b d
    | length n < 2 || length n > 5 =
        fail ("invalid reference point=" ++ n ++ " expected 2 to 5 [A-Z] characters")
    | b < 0 || b > 359 = fail ("invalid bearing=" ++ show b)
    | d < 0 = fail ("invalid distance=" ++ show d)
    | otherwise = return (BearingDistance n b d)

-- | 'Position' 'SignificantPoint' smart constructor. Fails if the given latitude
-- and/or longitude are not valid.
mkPosition :: (Monad m) => Float -> Float -> m SignificantPoint
mkPosition lat long
    | lat < -90.0 || lat > 90.0 = fail ("invalid latitude=" ++ show lat)
    | long < -180.0 || long > 180.0 = fail ("invalid longitude=" ++ show long)
    | otherwise = return (Position lat long)

namedPointParser :: Parser SignificantPoint
namedPointParser = do
    n <- word
    bd <- optional bearingDistanceParser
    case bd of
        Nothing -> mkCodedDesignator n
        Just (b, d) -> mkBearingDistance n b d

latLongParser :: Parser SignificantPoint
latLongParser = do
    latDeg <- natural 2
    latMin <- optional (natural 2)
    h <- oneOf "NS"
    longDeg <- natural 3
    longMin <- optional (natural 2)
    m <- oneOf "EW"
    do
        latDec <- decimal latDeg (fromMaybe 0 latMin) (north h)
        longDec <- decimal longDeg (fromMaybe 0 longMin) (east m)
        mkPosition latDec longDec

bearingDistanceParser :: Parser (Int, Int)
bearingDistanceParser = do
    b <- natural 3
    d <- natural 3
    return (b, d)

north :: Char -> Bool
north h = h == 'N'

east :: Char -> Bool
east m = m == 'E'

decimal :: (Monad m) => Int -> Int -> Bool -> m Float
decimal dd mm sign
    | mm > 59 = fail ("invalid minute=" ++ show mm)
    | sign = return dec
    | otherwise = return (-dec)
  where
    dec = fromIntegral dd + fromIntegral mm / 60.0
