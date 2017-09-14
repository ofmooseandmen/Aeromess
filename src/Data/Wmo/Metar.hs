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

-- | Wind direction in degrees.
newtype WindDirection =
    WindDirection Int
    deriving (Eq, Show)

-- | Variable wind direction.
data VariableDirection = VariableDirection
    { left :: WindDirection -- ^ the left extreme of wind direction.
    , right :: WindDirection -- ^ the right extreme of wind direction.
    } deriving (Eq, Show)

-- | Wind speed in appropriate unit.
data WindSpeed
    = Kt Int -- ^ knots
    | Kmh Int -- ^ kilometres per hour
    | Mps Int -- ^ meters per hour
    deriving (Eq, Show)

-- | Wind group data.
-- direction `000` and speed `00` indicates calm conditions.
data Wind
    = Calm
    | Wind { direction :: Maybe WindDirection -- ^ mean true direction in degrees rounded off to the nearest 10 degrees
                                              -- from which the wind is blowing, when absent the direction is variable
           , speed :: WindSpeed -- ^ mean speed of the wind over the 10-minute period immediately preceding the observation.
           , gust :: Maybe WindSpeed -- ^ maximum gust wind speed if relevant.
           , variation :: Maybe VariableDirection -- ^ variable wind direction if relevant.
            }
    deriving (Eq, Show)

-- | Visibility tendency at the runway.
data VisibilityTendency
    = Down
    | Up
    | NoChange
    deriving (Eq, Show)

-- | Visibility distance in meters.
newtype VisibilityDistance =
    VisibilityDistance Int
    deriving (Eq, Show)

-- | Runway visual range data.
data RunwayVisualRange = RunwayVisualRange
    { designator :: String -- ^ runway designator, possibility appended with L(eft) C(entral)
                           -- or R(ight) for parallel runways.
    , minVisibility :: VisibilityDistance -- ^ visibility in meter or minimum visibility if it varies significantly.
    , maxVisibility :: Maybe VisibilityDistance -- ^ maximum visibility in meter, if visibility varies significantly.
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
    { horizontal :: VisibilityDistance -- ^ horizontal visibility in meters, 9999 indicates a visibility over 10 km.
    , directionalVariation :: [VisibilityDistance] -- ^ directional variation of the visibility.
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

-- | Cloud height in meters.
newtype CloudHeight =
    CloudHeight Int
    deriving (Eq, Show)

-- | Clouds group data.
data Clouds
    = Few { height :: CloudHeight
          , cType :: Maybe CloudType }
    | Scattered { height :: CloudHeight
                , cType :: Maybe CloudType }
    | Broken { height :: CloudHeight
             , cType :: Maybe CloudType }
    | Overcast { height :: CloudHeight
               , cType :: Maybe CloudType }
    | Obscured { verticalVisibility :: CloudHeight }
    | SkyClear
    | NoCloudBelow1500
    | NoCloudBelow3600 -- ^ used in automatic observations.
    deriving (Eq, Show)

-- | Mean sea level pressure.
data Pressure
    = Hpa Int -- ^ in hecto Pascals (standard)
    | InHg Int -- ^ in inches of mercury, tens, units, tenths, and hundredths (US)
    deriving (Eq, Show)

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

-- | Speed unit.
data SpeedUnit
    = KT -- ^ knots
    | MPS -- meters per second
    | KMH -- kilometers per hour
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Mean sea level pressure unit.
data PressureUnit
    = Q -- ^ hPa (standard)
    | A -- ^ inches of mercury (US)
    deriving (Bounded, Enum, Eq, Read, Show)

-- | 'WindDirection' parser
wdParser :: Parser WindDirection
wdParser = natural 3 >>= mkWindDirection

-- | 'VariableDirection' parser.
variableDirectionParser :: Parser VariableDirection
variableDirectionParser = do
    _ <- space
    l <- wdParser
    _ <- char 'V'
    r <- wdParser
    return (VariableDirection l r)

-- | if VRB -> Nothing, else parseDegrees.
windDirectionParser :: Parser (Maybe WindDirection)
windDirectionParser = do
    var <- fmap isJust (optional (string "VRB"))
    if var
        then return Nothing
        else fmap Just wdParser

-- | 'Calm' parser.
calmParser :: Parser Wind
calmParser = do
    _ <- string "00000"
    _ <- enumeration :: Parser SpeedUnit
    return Calm

-- | 'WindSpeed' from given speed and unit
speedFrom :: Int -> SpeedUnit -> WindSpeed
speedFrom s KT = Kt s
speedFrom s MPS = Mps s
speedFrom s KMH = Kmh s

-- | 'Wind' parser.
windParser :: Parser Wind
windParser = do
    d <- windDirectionParser
    s <- natural 2
    g <- optional (char 'G' >> natural 2)
    u <- enumeration :: Parser SpeedUnit
    v <- optional (try variableDirectionParser)
    return (Wind d (speedFrom s u) (fmap (`speedFrom` u) g) v)

-- | 'Visibility' parser.
visibilityParser :: Parser Visibility
visibilityParser = do
    v <- natural 4
    return (Visibility (VisibilityDistance v) [] Nothing [])

-- | 'WindDirection' smart constructor. Fails if given integer is outside [0 .. 359].
mkWindDirection :: (Monad m) => Int -> m WindDirection
mkWindDirection n
    | n < 0 || n > 359 = fail ("invalid degrees=" ++ show n)
    | otherwise = return (WindDirection n)

-- | CAVOK or visibility, weather and clouds parser.
-- returns nothing if CAVOK
vwcParser :: Parser (Maybe (Maybe Visibility, [Weather], [Clouds]))
vwcParser = do
    ok <- fmap isJust (optional (string "CAVOK"))
    if ok
        then return Nothing
        else do
            vs <- visibilityParser
            return (Just (Just vs, [], []))

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
    vwc <- vwcParser
    let ok = isNothing vwc
    let (vs, we, cl) = fromMaybe (Nothing, [], []) vwc
    return (Metar rt (cor1 || cor2) au ms st dt wd ok vs we cl Nothing Nothing Nothing Nothing)

-- | Parses the given textual representation of a 'Metar'.
-- return either an 'Error' ('Left') or the parsed 'Metar' ('Right').
parse :: String -> Either Error Metar
parse = runParser parser
