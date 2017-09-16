-- | This module provides tools to parse and format METAR.
-- METARs are aerodrome routine meteorological reports.
-- This module is compliant with WMO Technical Regulations - Annex II, but
-- best efforts have been made to cater for small deviations mostly for US and Canadian
-- METARs as described in the FAA Aeronautical Information Manual.
--
-- == /Relevant links/
-- - <http://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI1/Publications/2016update/WMO306_vI1_en_2011UP2016.pdf WMO Technical Regulations - Annex II>
-- - <https://www.faa.gov/air_traffic/publications/media/aim.pdf>
-- - <https://en.wikipedia.org/wiki/METAR METAR on Wikipedia>
-- - <http://sto.iki.fi/metar>
--
module Data.Wmo.Metar where

import Data.Aeromess.Parser
import Data.Char (isDigit)
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time
import Data.Maybe

-- | type of report.
data Type
    = METAR -- ^ periodic report, e.g. generated once an hour or half hour.
    | SPECI -- ^ special report issued when conditions have significantly changed.
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
    = WindSpeedKt Int -- ^ knots.
    | WindSpeedKmh Int -- ^ kilometres per hour.
    | WindSpeedMps Int -- ^ metres per hour.
    deriving (Eq, Show)

-- | Wind group data.
-- direction `000` and speed `00` indicates calm conditions.
data Wind
    = Calm
    | Wind { direction :: Maybe WindDirection -- ^ mean true direction in degrees rounded off to the nearest 10 degrees
                                              -- from which the wind is blowing, when absent the direction is variable
          ,  speed :: WindSpeed -- ^ mean speed of the wind over the 10-minute period immediately preceding the observation.
          ,  gust :: Maybe WindSpeed -- ^ maximum gust wind speed if relevant.
          ,  variation :: Maybe VariableDirection -- ^ variable wind direction if relevant.
           }
    deriving (Eq, Show)

-- | Visibility tendency at the runway.
data VisibilityTendency
    = Down
    | Up
    | NoChange
    deriving (Eq, Show)

-- | Visibility distance in appropriate unit.
data VisibilityDistance
    = VisibilityDistanceMetre Int -- ^ metre, standard unit.
    | VisibilityDistanceFeet Int -- ^ feet, used by FAA for RVR
    | VisibilityDistanceMile { unit :: Maybe Int -- ^ mile, formally statute mile, used by US/Canada.
                            ,  fraction :: Maybe (Int, Int) -- ^ mile fraction
                             }
    deriving (Eq, Show)

-- | Runway designator: 2 digits possibility appended with L(eft) C(entral) or R(ight) for parallel runways.
newtype RunwayDesignator =
    RunwayDesignator String
    deriving (Eq, Show)

-- | indicates an extreme value of runway visual range.
data ExtrmeRvr
    = Lower
    | Higher
    deriving (Eq, Show)

-- | Runway visual range data.
data RunwayVisualRange = RunwayVisualRange
    { designator :: RunwayDesignator -- ^ runway designator
    , meanVisibility :: VisibilityDistance -- ^ mean visibility in metre.
    , isOutsideMeasuringRange :: Maybe ExtrmeRvr -- ^ present only if the RVR values are outside the measuring range of the observing system.
    , visibilityTendency :: Maybe VisibilityTendency -- ^ visibility tendency.
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
    | NorthWest
    deriving (Eq, Show)

-- | Visibility group data.
data Visibility = Visibility
    { prevailing :: VisibilityDistance -- ^ prevailing horizontal visibility in metres, 9999 indicates a visibility over 10 km.
    , lowest :: Maybe VisibilityDistance -- ^ lowest visibility if reported.
    , lowestDirection :: Maybe CompassPoint -- ^ lowest visibility.
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

-- | Cloud height in metres.
newtype CloudHeight =
    CloudHeight Int
    deriving (Eq, Show)

data CloudAmount = CloudAmount
    { height :: CloudHeight -- ^ height of cloud base
    , cType :: Maybe CloudType -- ^ type of clouds if relevant
    } deriving (Eq, Show)

-- | Clouds group data.
data Clouds
    = Few CloudAmount -- ^ few amount of clouds.
    | Scattered CloudAmount -- ^ Scattered amount of clouds.
    | Broken CloudAmount -- ^ broken amount of clouds.
    | Overcast CloudAmount -- ^ overcast amount of clouds.
    | Obscured CloudHeight -- ^ sky obscured, gives vertical visibility.
    | SkyClear
    | NoCloudBelow1500
    | NoCloudBelow3600 -- ^ used in automatic observations.
    deriving (Eq, Show)

-- | Mean sea level pressure.
data Pressure
    = Hpa Int -- ^ in hecto Pascals (standard).
    | InHg Int -- ^ in inches of mercury, tens, units, tenths, and hundredths (US).
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
    = KT -- ^ knots.
    | MPS -- metres per second.
    | KMH -- kilometres per hour.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Mean sea level pressure unit.
data PressureUnit
    = Q -- ^ hPa (standard).
    | A -- ^ inches of mercury (US).
    deriving (Bounded, Enum, Eq, Read, Show)

-- | compass point code
data CompassPointCode
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW
    deriving (Bounded, Enum, Eq, Read, Show)

-- | 'WindDirection' parser.
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

-- | 'WindSpeed' from given speed and unit.
speedFrom :: Int -> SpeedUnit -> WindSpeed
speedFrom s KT = WindSpeedKt s
speedFrom s MPS = WindSpeedMps s
speedFrom s KMH = WindSpeedKmh s

-- | 'Wind' parser.
-- wind direction on 3 digits, degrees
-- speed on 2 digits
-- optionally gust speed on 2 digits
-- speed unit.
windParser :: Parser Wind
windParser = do
    d <- windDirectionParser
    s <- natural 2
    g <- optional (char 'G' >> natural 2)
    u <- enumeration :: Parser SpeedUnit
    v <- optional (try variableDirectionParser)
    return (Wind d (speedFrom s u) (fmap (`speedFrom` u) g) v)

-- | 'RunwayVisualRange' parser.
-- R followed by runway name (2 digits followed optionally by L, R or C)
-- followed by optionally M or P
-- followed by mean visibility (metre or feet)
-- followed by optionally tendency
rvrParser :: Parser RunwayVisualRange
rvrParser = do
    r <- string "R" >> stringTill '/' >>= mkRunwayDesignator
    _o <- optional (oneOf "MP")
    o <-
        case _o of
            Just 'M' -> return (Just Lower)
            Just 'P' -> return (Just Higher)
            _ -> return Nothing
    _d <- natural 4
    u <- optional (string "FT")
    d <- case u of
        Just "FT" -> return (VisibilityDistanceFeet (100 * _d))
        _ -> return (VisibilityDistanceMetre _d)
    _t <- optional (oneOf "UDN")
    t <-
        case _t of
            Just 'U' -> return (Just Up)
            Just 'D' -> return (Just Down)
            Just 'N' -> return (Just NoChange)
            _ -> return Nothing
    _ <- space
    return (RunwayVisualRange r d o t)

-- | parser of a list of 'RunwayVisualRange'.
rvrsParser :: Parser [RunwayVisualRange]
rvrsParser = many (try rvrParser)

compassPointParser :: Parser CompassPoint
compassPointParser = do
    c <- enumeration :: Parser CompassPointCode
    case c of
        N -> return North
        NE -> return NorthEast
        E -> return East
        SE -> return SouthEast
        S -> return South
        SW -> return SouthWest
        W -> return West
        NW -> return NorthWest

-- | WMO visibility: prevailing visibility on 4 digits in metres
-- followed by optionally a space and 4 digits indicating the lowest visibility
-- followed by optionally the direction for which the lowest visibility has been observed
-- followed by runway visibility
wmoVisibilityParser :: Parser Visibility
wmoVisibilityParser = do
    v <- natural 4
    _ <- space
    l <- optional (natural 4)
    d <-
        case l of
            Nothing -> return Nothing
            Just _ -> fmap Just (compassPointParser <* space)
    r <- rvrsParser
    return (Visibility (VisibilityDistanceMetre v) (fmap VisibilityDistanceMetre l) d r)

-- | 'x/ySM' parser.
mileFractionParser :: Parser (Int, Int)
mileFractionParser = do
    n <- natural 1
    _ <- slash
    d <- natural 1
    _ <- string "SM"
    return (n, d)

-- | FAA visibility: only prevailing visibility in miles and fraction ending with SM
-- at least mile (1 digit) or fraction (1 digit / 1 digit) present.
faaVisibilityParser :: Parser Visibility
faaVisibilityParser = do
    m <- try (optional ((natural 2 <|> natural 1) <* (string "SM" <|> string " ")))
    f <- optional mileFractionParser
    r <- try rvrsParser
    let p = VisibilityDistanceMile m f
    return (Visibility p Nothing Nothing r)

-- | 'Visibility' parser.
-- FAA deviates from the WMO standard here.
visibilityParser :: Parser Visibility
visibilityParser = try wmoVisibilityParser <|> faaVisibilityParser

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

-- | 'RunwayDesignator' smart constructor. Fails if given string is not a valid designator.
mkRunwayDesignator
    :: (Monad m)
    => String -> m RunwayDesignator
mkRunwayDesignator s
    | length s /= 2 && length s /= 3 = fail ("invalid runway designator=" ++ s)
    | not (all isDigit (take 2 s)) = fail ("invalid runway designator=" ++ s)
    | length s == 3 && (last s /= 'C' && last s /= 'R' && last s /= 'L') =
        fail ("invalid runway designator=" ++ s)
    | otherwise = return (RunwayDesignator s)

-- | 'WindDirection' smart constructor. Fails if given integer is outside [0 .. 359].
mkWindDirection
    :: (Monad m)
    => Int -> m WindDirection
mkWindDirection n
    | n < 0 || n > 359 = fail ("invalid degrees=" ++ show n)
    | otherwise = return (WindDirection n)

-- | 'Metar' parser.
parser :: Parser Metar
parser = do
    rt <- enumeration :: Parser Type
    -- WMO allow COR here
    cor1 <- fmap isJust (optional (try (string " COR")))
    _ <- space
    -- station.
    st <- aerodromeParser
    -- WMO allows NIL or AUTO, some other organisations also allow COR
    ms <- fmap isJust (optional (try (string " NIL")))
    au <- fmap isJust (optional (try (string " AUTO")))
    cor2 <- fmap isJust (optional (try (string " COR")))
    _ <- space
    -- day and time
    dt <- dayTimeParser
    _ <- char 'Z'
    _ <- space
    -- wind, either all 00000 (calm) or data
    wd <- calmParser <|> windParser
    _ <- space
    -- cavok or visibility, weather and clouds
    vwc <- vwcParser
    let ok = isNothing vwc
    let (vs, we, cl) = fromMaybe (Nothing, [], []) vwc
    -- TODO, once everything is parsed, check for '=' or end of line.
    return (Metar rt (cor1 || cor2) au ms st dt wd ok vs we cl Nothing Nothing Nothing Nothing)

-- | Parses the given textual representation of a 'Metar'.
-- return either an 'Error' ('Left') or the parsed 'Metar' ('Right').
parse :: String -> Either Error Metar
parse = runParser parser
