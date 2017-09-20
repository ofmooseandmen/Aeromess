-- |
-- Provides data types and functions pertaining to locations
-- in accordance with the ICAO 4444 edition 2016 standard.
module Data.Icao.Location
    (
    -- * Data
      Aerodrome
    , Bearing
    , Distance
    , Latitude
    , Longitude
    , PointNameCode
    , GeographicPosition(..)
    , BearingDistancePosition(..)
    , SignificantPoint(CodedDesignator, Position, BearingDistance)
    -- * Parsers
    , aerodromeParser
    , significantPointParser
    , parseAerodrome
    , parseSignificantPoint
    -- * Smart constructors
    , mkAerodrome
    , mkCodedDesignator
    , mkPosition
    , mkBearingDistance
    ) where

import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Aeromess.Parser
import Data.Char
import Data.Either ()
import Data.Maybe

-- | the name of an aerodrome, 4 uppercase characters.
newtype Aerodrome =
    Aerodrome String
    deriving (Eq, Show)

-- | Bearing in degrees magnetic, in range [0 .. 359].
newtype Bearing =
    Bearing Int
    deriving (Eq, Show)

-- | Distance in nautical miles, always positive.
newtype Distance =
    Distance Int
    deriving (Eq, Show)

-- | Latitude in decimal degrees, in range [-90 .. 90].
newtype Latitude =
    Latitude Float
    deriving (Eq, Show)

-- | Longitude in decimal degrees, in range [-180 .. 180].
newtype Longitude =
    Longitude Float
    deriving (Eq, Show)

-- | 5 letters pronounceable 'name-code' (5LNC) to designate a Significant Point.
newtype PointNameCode =
    PointNameCode String
    deriving (Eq, Show)

-- | Position expressed by the decimal latitude and longitude.
data GeographicPosition = GeographicPosition
    { latitude :: Latitude
    , longitude :: Longitude
    } deriving (Eq, Show)

-- | Position expressed as a bearing and distance from a reference point.
data BearingDistancePosition = BearingDistancePosition
    { reference :: PointNameCode -- ^ 'name-code' of the reference point.
    , bearing :: Bearing -- ^ Bearing from the reference point
    , distance :: Distance -- ^ Distance from the reference point
    } deriving (Eq, Show)

-- | a significant point
data SignificantPoint
    -- | Point coded designator (e.g. 'RASMU'), 2 to 5 characters.
    = CodedDesignator PointNameCode
    -- | Point Position; latitude and longitude in decimal degrees.
    | Position GeographicPosition
    -- | Bearing and distance from a reference point, bearing in degrees, distance in nautical miles.
    | BearingDistance BearingDistancePosition
    deriving (Eq, Show)

-- | 'Aerodrome' Parser.
aerodromeParser :: Parser Aerodrome
aerodromeParser = do
    a <- word
    mkAerodrome a

-- | Parses the given textual representation of an 'Aerodrome'.
-- return either an error message ('Left') or the parsed 'Aerodrome' ('Right').
parseAerodrome :: String -> Either String Aerodrome
parseAerodrome = runParser aerodromeParser

-- | 'Aerodrome' smart constructor. Fails if the given name is not a valid.
mkAerodrome
    :: (MonadFail m)
    => String -> m Aerodrome
mkAerodrome n
    | length n /= 4 || not (all isUpper n) =
        fail ("invalid aerodrome name=" ++ n ++ " expected 4 [A-Z] characters")
    | otherwise = return (Aerodrome n)

-- | 'SignificantPoint' Parser.
significantPointParser :: Parser SignificantPoint
significantPointParser = namedPointParser <|> latLongParser

-- | Parses the given textual representation of a 'SignificantPoint'.
-- return either an error message ('Left') or the parsed 'SignificantPoint' ('Right').
parseSignificantPoint :: String -> Either String SignificantPoint
parseSignificantPoint = runParser significantPointParser

-- | 'CodedDesignator' 'SignificantPoint' smart constructor. Fails if the given name
-- is not a valid.
mkCodedDesignator
    :: (MonadFail m)
    => String -> m SignificantPoint
mkCodedDesignator n = fmap CodedDesignator (mkPointNameCode n)

-- | 'BearingDistance' 'SignificantPoint' smart constructor. Fails if the given name
-- and/or bearing and/or distance are not a valid.
mkBearingDistance
    :: (MonadFail m)
    => String -> Int -> Int -> m SignificantPoint
mkBearingDistance n b d = do
    ref <- mkPointNameCode n
    br <- mkBearing b
    di <- mkDistance d
    return (BearingDistance (BearingDistancePosition ref br di))

-- | 'Position' 'SignificantPoint' smart constructor. Fails if the given latitude
-- and/or longitude are not valid.
mkPosition
    :: (MonadFail m)
    => Float -> Float -> m SignificantPoint
mkPosition lat long = do
    la <- mkLatitude lat
    lo <- mkLongitude long
    return (Position (GeographicPosition la lo))

mkBearing
    :: (MonadFail m)
    => Int -> m Bearing
mkBearing b
    | b < 0 || b > 359 = fail ("invalid bearing=" ++ show b)
    | otherwise = return (Bearing b)

mkDistance
    :: (MonadFail m)
    => Int -> m Distance
mkDistance d
    | d < 0 = fail ("invalid distance=" ++ show d)
    | otherwise = return (Distance d)

mkLatitude
    :: (MonadFail m)
    => Float -> m Latitude
mkLatitude lat
    | lat < -90.0 || lat > 90.0 = fail ("invalid latitude=" ++ show lat)
    | otherwise = return (Latitude lat)

mkLongitude
    :: (MonadFail m)
    => Float -> m Longitude
mkLongitude long
    | long < -180.0 || long > 180.0 = fail ("invalid longitude=" ++ show long)
    | otherwise = return (Longitude long)

mkPointNameCode
    :: (MonadFail m)
    => String -> m PointNameCode
mkPointNameCode n
    | length n < 2 || length n > 5 =
        fail ("invalid coded designator=" ++ n ++ " expected 2 to 5 [A-Z] characters")
    | otherwise = return (PointNameCode n)

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
    do latDec <- decimal latDeg (fromMaybe 0 latMin) (north h)
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

decimal
    :: (MonadFail m)
    => Int -> Int -> Bool -> m Float
decimal dd mm sign
    | mm > 59 = fail ("invalid minute=" ++ show mm)
    | sign = return dec
    | otherwise = return (-dec)
  where
    dec = fromIntegral dd + fromIntegral mm / 60.0
