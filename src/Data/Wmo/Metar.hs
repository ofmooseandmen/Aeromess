-- | This module provides tools to parse and format METAR.
-- METAR is a format for reporting weather information.
-- This module is compliant with WMO Technical Regulations - Annex II, but
-- best effort have been made to cater for small deviations (mostly US and Canadian METAR).
--
-- == /Relevant links/
-- - <http://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI1/Publications/2016update/WMO306_vI1_en_2011UP2016.pdf WMO Technical Regulations - Annex II>
-- - <https://en.wikipedia.org/wiki/METAR METAR on Wikipedia>
-- - <http://sto.iki.fi/metar>
--
module Data.Wmo.Metar where

import Data.Aeromess.Parser
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time
import Data.Maybe

-- | type of report.
data Type
    = METAR -- ^ periodic report, e.g. generated once an hour or half hour
    | SPECI -- ^ special report issued when conditions have significantly changed
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Degrees in range [0 .. 359].
newtype Degrees =
    Degrees Int
    deriving (Eq, Show)

-- | Speed unit.
data SpeedUnit
    = KT -- ^ knots
    | MPS -- meters per second
    | KMH -- kilometers per hour
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Variable wind direction.
data VariableDirection = VariableDirection
    { left :: Degrees -- ^ the left extreme of wind direction.
    , right :: Degrees -- ^ the right extreme of wind direction.
    } deriving (Eq, Show)

-- | Wind group data.
-- direction `000` and speed `00` indicates calm conditions.
data Wind
    = Calm
    | Wind { direction :: Maybe Degrees -- ^ mean true direction in degrees rounded off to the nearest 10 degrees
                                 -- from which the wind is blowing, when absent the direction is variable
           , speed :: Natural2 -- ^ mean speed of the wind over the 10-minute period immediately preceding the observation.
           , gust :: Maybe Natural2 -- ^ maximum gust wind speed if relevant.
           , speedUnit :: SpeedUnit -- ^ speed unit.
           , variation :: Maybe VariableDirection -- ^ variable wind direction if relevant.
            }
    deriving (Eq, Show)

-- | Visibility tendency at the runway.
data VisibilityTendency
    = Down
    | Up
    | NoChange
    deriving (Eq, Show)

-- | Runway visual range data.
data RunwayVisualRange = RunwayVisualRange
    { designator :: String -- ^ runway designator, possibility appended with L(eft) C(entral)
                           -- or R(ight) for parallel runways.
    , minVisibility :: Natural4 -- ^ visibility in meter or minimum visibility if it varies significantly.
    , maxVisibility :: Maybe Natural4 -- ^ maximum visibility in meter, if visibility varies significantly.
    , visibilityTendency :: Maybe VisibilityTendency
    } deriving (Eq, Show)

-- | the 8 compass points.
data CompassPoint
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    deriving (Eq, Show)

-- | Visibility group data.
data Visibility = Visibility
    { horizontal :: Natural4 -- ^ horizontal visibility in meters, 9999 indicates a visibility over 10 km.
    , directionalVariation :: [Natural4] -- ^ directional variation of the visibility.
    , significantDirection :: Maybe CompassPoint -- ^ most operational significant direction.
    , runways :: [RunwayVisualRange] -- ^ visual range for each runway
    } deriving (Eq, Show)

-- | Weather qualifier.
data WeatherQualifier
    = Light -- ^ light weather.
    | Heavy -- ^ heavy weather.
    | Vicinity -- in vicinity, i.e. not on location but within 8000 m.
    deriving (Eq, Show)

-- | Weather descriptor.
data WeatherDescriptor
    = Shallow
    | Patches
    | Partial
    | Drifting
    | Blowing
    | Showers
    | Thunderstorm
    | Freezing
    deriving (Eq, Show)

-- | Weather phenomenon.
-- Precipitation: 'Drizzle' to 'UnknownPrecipitation'.
-- Obscuration  : 'Mist' to 'Spray'.
data WeatherPhenomenon
    = Drizzle
    | Rain
    | Snow
    | SnowGrains
    | IceCrystals
    | IcePellets
    | Hail
    | SmallHail
    | UnknownPrecipitation
    | Mist -- ^ visibility > 1000 m.
    | Fog -- ^ visibility < 1000 m.
    | Smoke
    | VolcanicAsh
    | WidespreadDust
    | Sand
    | Haze
    | Spray
    | Dust -- ^ well developed dust/sand whirls.
    | Squall
    | FunnelCloud -- ^ tornado, waterspout are always +FC, i.e. heavy FunnelCloud.
    | Sandstorm
    | DustStorm
    deriving (Eq, Show)

-- | Weather observation.
data Weather = Weather
    { qualifier :: Maybe WeatherQualifier -- ^ qualifier, when absent denotes moderate weather.
    , descriptor :: Maybe WeatherDescriptor -- ^ descriptor, if applicable.
    , phenomenon :: [WeatherPhenomenon] -- ^phenomenon(s).
    } deriving (Eq, Show)

-- | Cloud type.
data CloudType
    = Cumulonimbus
    | ToweringCumulus
    deriving (Eq, Show)

-- | Clouds group data.
-- height of cloud base and vertical visibility are expressed in meter.
data Clouds
    = Few { height :: Natural3
          , cType :: Maybe CloudType }
    | Scattered { height :: Natural3
                , cType :: Maybe CloudType }
    | Broken { height :: Natural3
             , cType :: Maybe CloudType }
    | Overcast { height :: Natural3
               , cType :: Maybe CloudType }
    | Obscured { verticalVisibility :: Natural3 }
    | SkyClear
    | NoCloudBelow1500
    | NoCloudBelow3600 -- ^ used in automatic observations.
    deriving (Eq, Show)

-- | Mean sea level pressure unit.
data PressureUnit
    = HectoPascal
    | Inches
    deriving (Eq, Show)

-- | Mean sea level pressure.
data Pressure = Pressure
    { value :: Natural4
    , unit :: PressureUnit
    } deriving (Eq, Show)

-- | METAR: an aerodrome routine meteorological report.
data Metar = Metar
    { reportType :: Type -- ^ type of the report.
    , corrected :: Bool -- ^ whether the report was corrected.
    , auto :: Bool -- ^ whether the report contains fully automated observations without human intervention.
    , missed :: Bool -- ^ whether the report corresponds to a missing report.
    , station :: Aerodrome -- ^ ICAO code for the observing station (an aerodrome).
    , when :: DayTime -- ^ when the observation was made.
    , wind :: Wind -- ^ wind related observations.
    , cavok :: Bool -- ^ whether ceiling and visibility are OK, in which case 'visibility', 'weather' and 'clouds' are empty.
    , visibility :: Maybe Visibility -- ^ visibility related observations.
    , weather :: [Weather] -- ^ weather related observations.
    , clouds :: [Clouds] -- ^ clouds related observations.
    , temperature :: Maybe Int -- ^ temperature rounded to nearest whole degree Celsius.
    , dewPoint :: Maybe Int -- ^ dew point rounded to nearest whole degree Celsius.
    , pressure :: Maybe Pressure -- ^ mean sea level pressure (“QNH”).
    , remarks :: Maybe FreeText -- ^ METAR components and miscellaneous abbreviations.
    } deriving (Eq, Show)

-- | 'Degrees' parser
degreesParser :: Parser Degrees
degreesParser = fmap Degrees (natural 3)

-- | 'VariableDirection' parser.
variableDirectionParser :: Parser VariableDirection
variableDirectionParser = do
    _ <- space
    l <- degreesParser
    _ <- char 'V'
    r <- degreesParser
    return (VariableDirection l r)

-- | if VRB -> Nothing, else parseDegrees.
windDirectionParser :: Parser (Maybe Degrees)
windDirectionParser = do
    var <- fmap isJust (optional (string "VRB"))
    if var
        then return Nothing
        else fmap Just degreesParser

-- | 'Calm' parser.
calmParser :: Parser Wind
calmParser = do
    _ <- string "00000"
    _ <- enumeration :: Parser SpeedUnit
    return Calm

-- | 'Wind' parser.
windParser :: Parser Wind
windParser = do
    d <- windDirectionParser
    s <- natural2Parser
    g <- optional (char 'G' >> natural2Parser)
    u <- enumeration :: Parser SpeedUnit
    v <- optional (try variableDirectionParser)
    return (Wind d s g u v)

-- | 'Visibility' parser.
visibilityParser :: Parser Visibility
visibilityParser = do
    v <- natural4Parser
    return (Visibility v [] Nothing [])

-- | 'Degrees' smart constructor. Fails if given integer is outside [0 .. 359].
mkDegrees :: (Monad m) => Int -> m Degrees
mkDegrees n
    | n < 0 || n > 359 = fail ("invalid degrees=" ++ show n)
    | otherwise = return (Degrees n)

-- | 'Metar' parser.
parser :: Parser Metar
parser = do
    rt <- enumeration :: Parser Type
    -- some format allow COR here...
    cor1 <- fmap isJust (optional (try (string " COR")))
    _ <- space
    st <- aerodromeParser
    ms <- fmap isJust (optional (try (string " NIL")))
    au <- fmap isJust (optional (try (string " AUTO")))
    cor2 <- fmap isJust (optional (try (string " COR")))
    _ <- space
    dt <- dayTimeParser
    _ <- char 'Z'
    _ <- space
    wd <- calmParser <|> windParser
    _ <- space
    ok <- fmap isJust (optional (string "CAVOK"))
    if ok
        -- visibility, weather and clouds groups are not present
        then return
                 (Metar
                      rt
                      (cor1 || cor2)
                      au
                      ms
                      st
                      dt
                      wd
                      True
                      Nothing
                      []
                      []
                      Nothing
                      Nothing
                      Nothing
                      Nothing)
        else do
            vs <- visibilityParser
            return
                (Metar
                     rt
                     (cor1 || cor2)
                     au
                     ms
                     st
                     dt
                     wd
                     False
                     (Just vs)
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)

-- | Parses the given textual representation of a 'Metar'.
-- return either an 'Error' ('Left') or the parsed 'Metar' ('Right').
parse :: String -> Either Error Metar
parse = runParser parser
