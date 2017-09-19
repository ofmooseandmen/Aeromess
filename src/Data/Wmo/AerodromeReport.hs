-- | This module provides tools to parse and format Meterological Aerordome Reports.
-- The WMO, which is the international authority for the code form, defines
-- /METAR/ as Aerodrome routine meteorological report and /SPECI/ as Aerodrome routine special report.
-- The FAA, on the other hand, uses aviation (routine|special) weather report.
--
-- This module is primarily compliant with WMO Technical Regulations - Annex II, but
-- best efforts have been made to cater for the deviations described in the FAA Aeronautical Information Manual.
--
-- == /Relevant links/
-- - <http://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI1/Publications/2016update/WMO306_vI1_en_2011UP2016.pdf WMO Technical Regulations - Annex II>
-- - <https://www.faa.gov/air_traffic/publications/media/aim.pdf>
-- - <https://en.wikipedia.org/wiki/Report Report on Wikipedia>
-- - <http://sto.iki.fi/Report>
--
-- TODO: break module when implementing TAF
-- TODO: support trend
--
module Data.Wmo.AerodromeReport
    ( module Data.Icao.Location
    , module Data.Icao.Time
    -- * Data
    , Type(..)
    , SpeedUnit(..)
    , WindDirection
    , WindVariableDirection(left, right)
    , WindSpeed
    , Wind(direction, speed, gust, variation)
    , VisibilityTendency(..)
    , VisibilityDistance
    , RunwayDesignator
    , ExtremeRvr(..)
    , RunwayVisualRange(..)
    , CompassPoint(..)
    , Visibility(..)
    , WeatherQualifier(..)
    , WeatherDescriptor(..)
    , WeatherPhenomenon(..)
    , Weather(..)
    , CloudType(..)
    , CloudHeight
    , CloudAmount(height, cType)
    , Clouds(..)
    , Pressure
    , Modifiers(..)
    , Report(..)
    , cavok
    -- * Builders
    , metar
    , speci
    , with
    , withModifiers
    , withWindDirection
    , withWindSpeed
    , withWindVariation
    , withPrevailingVisibility
    , withFaaPrevailingVisibility
    , withLowestVisibility
    , withRunwayVisualRange
    , withFaaRunwayVisualRange
    -- * Parsers
    , Parser
    , Error(message, column)
    , parser
    , parse
    ) where

import Data.Aeromess.Parser
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time
import Data.Maybe

-- | type of report.
data Type
    = METAR -- ^ periodic or routine report, e.g. generated once an hour or half hour.
    | SPECI -- ^ special report issued when conditions have significantly changed.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Speed unit.
data SpeedUnit
    = KT -- ^ knots.
    | MPS -- ^metres per second.
    | KMH -- ^ kilometres per hour.
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

-- | Wind direction in degrees.
newtype WindDirection =
    WindDirection Int
    deriving (Eq, Show)

-- | Variable wind direction.
data WindVariableDirection = WindVariableDirection
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
data Wind = Wind
    { direction :: Maybe WindDirection -- ^ mean true direction in degrees rounded off to the nearest 10 degrees
                                              -- from which the wind is blowing, when absent the direction is variable.
    , speed :: WindSpeed -- ^ mean speed of the wind over the 10-minute period immediately preceding the observation.
    , gust :: Maybe WindSpeed -- ^ maximum gust wind speed if relevant.
    , variation :: Maybe WindVariableDirection -- ^ variable wind direction if relevant.
    } deriving (Eq, Show)

-- | Visibility tendency at the runway.
data VisibilityTendency
    = Down
    | Up
    | NoChange
    deriving (Eq, Show)

-- | Visibility distance in appropriate unit.
data VisibilityDistance
    = VisibilityDistanceMetre Int -- ^ metre, standard unit.
    | VisibilityDistanceFeet Int -- ^ feet, used by FAA for RVR.
    | VisibilityDistanceMile { unit :: Maybe Int -- ^ mile, formally statute mile, used by US/Canada.
                            ,  fraction :: Maybe (Int, Int) -- ^ mile fraction.
                             }
    deriving (Eq, Show)

-- | Runway designator: 2 digits possibility appended with L(eft) C(entral) or R(ight) for parallel runways.
newtype RunwayDesignator =
    RunwayDesignator String
    deriving (Eq, Show)

-- | indicates an extreme value of runway visual range.
data ExtremeRvr
    = Lower
    | Higher
    deriving (Eq, Show)

-- | Runway visual range data.
data RunwayVisualRange = RunwayVisualRange
    { designator :: RunwayDesignator -- ^ runway designator.
    , meanVisibility :: VisibilityDistance -- ^ mean visibility in metre.
    , isOutsideMeasuringRange :: Maybe ExtremeRvr -- ^ present only if the RVR values are outside the measuring range of the observing system.
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
    , runways :: [RunwayVisualRange] -- ^ visual range for each runway.
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

-- | Amount of clouds.
data CloudAmount = CloudAmount
    { height :: CloudHeight -- ^ height of cloud base.
    , cType :: Maybe CloudType -- ^ type of clouds if relevant.
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

-- | Report modifiers.
data Modifiers = Modifiers
    { corrected :: Bool -- ^ whether the report was corrected.
    , auto :: Bool -- ^ whether the report contains fully automated observations without human intervention.
    , missed :: Bool -- ^ whether the report corresponds to a missing report.
    } deriving (Eq, Show)

-- | Report: an aerodrome routine or special meteorological report.
data Report = Report
    { reportType :: Type -- ^ type of the report.
    , modifiers :: Modifiers -- ^ modifiers (cor, auto, nil).
    , station :: Aerodrome -- ^ ICAO code for the observing station (an aerodrome).
    , when :: DayTime -- ^ when the observation was made.
    , wind :: Maybe Wind -- ^ wind related observations.
    , visibility :: Maybe Visibility -- ^ visibility related observations.
    , weather :: [Weather] -- ^ weather related observations.
    , clouds :: [Clouds] -- ^ clouds related observations.
    , temperature :: Maybe Int -- ^ temperature rounded to nearest whole degree Celsius.
    , dewPoint :: Maybe Int -- ^ dew point rounded to nearest whole degree Celsius.
    , pressure :: Maybe Pressure -- ^ mean sea level pressure (“QNH”).
    , remarks :: Maybe FreeText -- ^ Report components and miscellaneous abbreviations.
    } deriving (Eq, Show)

-- | Determines the given 'Report' reports Cloud And Visibility OK (CAVOK).
-- CAVOK is an abbreviation for Cloud And Visibility OK, indicating no cloud below 5,000 ft
-- (1,500 m) or the highest minimum sector altitude and no cumulonimbus or towering cumulus
-- at any level, a visibility of 10 km (6 mi) or more and no significant weather change.
cavok :: Report -> Bool
cavok m = isNothing (visibility m) && null (weather m) && null (clouds m)

-- | Builds a periodic 'Report' (METAR) for the given station and time and all given
-- setter.
--
-- If no @setters@ are provided, the METAR will contain no Wind (Calm conditions),
-- report 'cavok' conditions, ISA temperature (15 degrees Celsisus) and pressure (TODO value)
-- TODO default dew point???
-- TODO update defaultReport
--
-- See 'with' and @withXXXX@ functions.
--
-- The returned 'Monad' allows to extract the 'Report' using a 'Maybe' or 'Either'
-- monad
--
-- >  example =
-- >    Report "ESSA" (18, 21, 48) [withWindDirection 150, withWindSpeed 40 Nothing KT]
--
metar
    :: (Monad m)
    => String -> (Int, Int, Int) -> [m Report -> m Report] -> m Report
metar st dt = with (defaultReport METAR st dt)

-- | Builds a special 'Report' (SPECI) for the given station and time and all given
-- setter.
-- see 'metar'.
speci
    :: (Monad m)
    => String -> (Int, Int, Int) -> [m Report -> m Report] -> m Report
speci st dt = with (defaultReport SPECI st dt)

-- | Modifies the given @report@ by running each given setter.
-- Could be replaced with:
-- > with :: (Monad m) => [a -> m a] -> (a -> m a)
-- > with = foldl (>=>) return
with
    :: (Monad m)
    => m Report -> [m Report -> m Report] -> m Report
with v setters = v & foldl (.) id setters

-- | Modifies the given @report@ by setting the modifiers (COR, AUTO, MIS).
withModifiers
    :: (Monad m)
    => (Bool, Bool, Bool) -> m Report -> m Report
withModifiers modifs report = do
    let (cor, aut, mis) = modifs
    fmap (\mt -> mt {modifiers = Modifiers cor aut mis}) report

-- | Modifies the given @report@ by setting the wind direction.
withWindDirection
    :: (Monad m)
    => Int -> m Report -> m Report
withWindDirection dir report = do
    _dir <- mkWindDirection dir
    fmap (\mt -> mt {wind = windWithDirection _dir (wind mt)}) report

-- | Modifies the given @report@ by setting the wind speed (@spd@) and gust (@gst).
withWindSpeed
    :: (Monad m)
    => Int -> Maybe Int -> SpeedUnit -> m Report -> m Report
withWindSpeed spd gst ut report = do
    _spd <- mkWindSpeed spd ut
    _gst <-
        case gst of
            Nothing -> return Nothing
            Just s -> fmap Just (mkWindSpeed s ut)
    fmap (\mt -> mt {wind = windWithSpeed _spd _gst (wind mt)}) report

-- | Modifies the given @report@ by setting the wind variation.
withWindVariation
    :: (Monad m)
    => Int -> Int -> m Report -> m Report
withWindVariation lft rgt report = do
    _lft <- mkWindDirection lft
    _rgt <- mkWindDirection rgt
    fmap (\mt -> mt {wind = windWithVariation (WindVariableDirection _lft _rgt) (wind mt)}) report

-- | Modifies the given @report@ by setting the prevailing visibility according to the WMO standard.
-- the visibility is expressed in meters.
withPrevailingVisibility
    :: (Monad m)
    => Int -> m Report -> m Report
withPrevailingVisibility dst report = do
    _dst <- mkVisibilityDistanceMeter dst
    fmap (\mt -> mt {visibility = visiblityWithPrevailing _dst (visibility mt)}) report

-- | Modifies the given @report@ by setting the prevailing visibility according to the FAA standard.
-- the visibility is expressed in statue miles (@mile@ and @fraction@).
withFaaPrevailingVisibility
    :: (Monad m)
    => Maybe Int -> Maybe (Int, Int) -> m Report -> m Report
withFaaPrevailingVisibility Nothing Nothing _ =
    fail "invalid prevailing visibility, at least one of mile or fraction is required"
withFaaPrevailingVisibility m f report = do
    _dst <- mkVisibilityDistanceMile m f
    fmap (\mt -> mt {visibility = visiblityWithPrevailing _dst (visibility mt)}) report

-- | Modifies the given @report@ by setting the lowest visibility (meter) and for a direction (if any).
-- Note: this is not supported by the FAA standard.
withLowestVisibility
    :: (Monad m)
    => Int -> Maybe CompassPoint -> m Report -> m Report
withLowestVisibility dst cp report = do
    _dst <- mkVisibilityDistanceMeter dst
    fmap (\mt -> mt {visibility = visiblityWithLowest _dst cp (visibility mt)}) report

-- | Modifies the given @report@ by adding a runway visual range (RVR) according to the WMO standard.
-- the runway visibility is expressed in meters.
withRunwayVisualRange
    :: (Monad m)
    => String -> Int -> Maybe ExtremeRvr -> Maybe VisibilityTendency -> m Report -> m Report
withRunwayVisualRange rwy dst ext tdc report = do
    _rwy <- mkRunwayDesignator rwy
    _dst <- mkVisibilityDistanceMeter dst
    fmap (\mt -> mt {visibility = visiblityWithRvr _rwy _dst ext tdc (visibility mt)}) report

-- | Modifies the given @report@ by adding the runway visual range (RVR) according to the FAA standard.
-- the runway visibility is expressed in feet.
withFaaRunwayVisualRange
    :: (Monad m)
    => String -> Int -> Maybe ExtremeRvr -> Maybe VisibilityTendency -> m Report -> m Report
withFaaRunwayVisualRange rwy dst ext tdc report = do
    _rwy <- mkRunwayDesignator rwy
    _dst <- mkVisibilityDistanceFeet dst
    fmap (\mt -> mt {visibility = visiblityWithRvr _rwy _dst ext tdc (visibility mt)}) report

-- | 'Report' parser.
parser :: Parser Report
parser = do
    rt <- enumeration :: Parser Type
    -- WMO allows COR here
    cor1 <- fmap isJust (optional (try (string " COR")))
    _ <- space
    -- station
    st <- aerodromeParser
    _ <- space
    -- day and time
    dt <- dayTimeParser
    _ <- char 'Z'
    -- WMO allows NIL or AUTO and also COR
    ms <- fmap isJust (optional (try (string " NIL")))
    au <- fmap isJust (optional (try (string " AUTO")))
    cor2 <- fmap isJust (optional (try (string " COR")))
    _ <- space
    -- wind, either all 00000 (calm) or data
    wd <- calmParser <|> windParser
    _ <- space
    -- cavok or visibility, weather and clouds
    vwc <- vwcParser
    let (vs, we, cl) = fromMaybe (Nothing, [], []) vwc
    let m = Modifiers (cor1 || cor2) au ms
    -- TODO, once everything is parsed, check for '=' or end of line.
    return (Report rt m st dt wd vs we cl Nothing Nothing Nothing Nothing)

-- | Parses the given textual representation of a 'Report'.
-- return either an 'Error' ('Left') or the parsed 'Report' ('Right').
parse :: String -> Either Error Report
parse = runParser parser

-- ---------------------------
-- Private smart constructors.
-- ---------------------------

noModifiers :: Modifiers
noModifiers = Modifiers False False False

mkRunwayDesignator
    :: (Monad m)
    => String -> m RunwayDesignator
mkRunwayDesignator s
    | length s /= 2 && length s /= 3 = fail ("invalid runway designator=" ++ s)
    | not (all isDigit (take 2 s)) = fail ("invalid runway designator=" ++ s)
    | length s == 3 && (last s /= 'C' && last s /= 'R' && last s /= 'L') =
        fail ("invalid runway designator=" ++ s)
    | otherwise = return (RunwayDesignator s)

mkWindSpeed
    :: (Monad m)
    => Int -> SpeedUnit -> m WindSpeed
mkWindSpeed s u
    | s < 0 || s > 99 = fail ("invalid wind speed=" ++ show s)
    | otherwise = return (speedFrom s u)

-- | 'WindSpeed' from given speed and unit.
speedFrom :: Int -> SpeedUnit -> WindSpeed
speedFrom s KT = WindSpeedKt s
speedFrom s MPS = WindSpeedMps s
speedFrom s KMH = WindSpeedKmh s

mkWindDirection
    :: (Monad m)
    => Int -> m WindDirection
mkWindDirection d
    | d < 0 || d > 359 = fail ("invalid degrees=" ++ show d)
    | otherwise = return (WindDirection d)

mkVisibilityDistanceMeter :: (Monad m) => Int -> m VisibilityDistance
mkVisibilityDistanceMeter m
    | m < 0 || m > 9999 = fail ("invalid distance [meter]=" ++ show m)
    | otherwise = return (VisibilityDistanceMetre m)

mkVisibilityDistanceMile :: (Monad m) => Maybe Int -> Maybe (Int, Int) -> m VisibilityDistance
mkVisibilityDistanceMile m f
    | maybe False (< 0) m || maybe False (> 99) m =
        fail ("invalid distance [mile]=" ++ show m)
    | maybe False (< 0) (fmap fst f) || maybe False (> 9) (fmap fst f) =
        fail ("invalid distance [fraction]=" ++ show f)
    | maybe False (< 0) (fmap snd f) || maybe False (> 9) (fmap snd f) =
        fail ("invalid distance [fraction]=" ++ show f)
    | otherwise = return (VisibilityDistanceMile m f)

mkVisibilityDistanceFeet :: (Monad m) => Int -> m VisibilityDistance
mkVisibilityDistanceFeet m
    | m < 0 || m > 9999 = fail ("invalid distance [feet]=" ++ show m)
    | otherwise = return (VisibilityDistanceFeet m)

-- ------------------------
-- Private builder helpers.
-- -------------------------
-- | default 'Report' of given type for given station and time
-- No wind, and CAVOK conditions.
defaultReport
    :: (Monad m)
    => Type -> String -> (Int, Int, Int) -> m Report
defaultReport t st dt = do
    _st <- mkAerodrome st
    let (d, h, m) = dt
    _dt <- mkDayTime d h m
    return (Report t noModifiers _st _dt Nothing Nothing [] [] Nothing Nothing Nothing Nothing)

windWithDirection :: WindDirection -> Maybe Wind -> Maybe Wind
windWithDirection dir Nothing = Just (Wind (Just dir) (WindSpeedKt 0) Nothing Nothing)
windWithDirection dir (Just wd) = Just (wd {direction = Just dir})

windWithSpeed :: WindSpeed -> Maybe WindSpeed -> Maybe Wind -> Maybe Wind
windWithSpeed spd gst Nothing = Just (Wind Nothing spd gst Nothing)
windWithSpeed spd gst (Just wd) = Just (wd {speed = spd, gust = gst})

windWithVariation :: WindVariableDirection -> Maybe Wind -> Maybe Wind
windWithVariation var Nothing = Just (Wind Nothing (WindSpeedKt 0) Nothing (Just var))
windWithVariation var (Just wd) = Just (wd {variation = Just var})

visiblityWithPrevailing :: VisibilityDistance -> Maybe Visibility -> Maybe Visibility
visiblityWithPrevailing dst Nothing = Just (Visibility dst Nothing Nothing [])
visiblityWithPrevailing dst (Just v) = Just (v {prevailing = dst})

visiblityWithLowest :: VisibilityDistance -> Maybe CompassPoint -> Maybe Visibility -> Maybe Visibility
visiblityWithLowest dst cp Nothing = Just (Visibility (VisibilityDistanceMetre 0) (Just dst) cp [])
visiblityWithLowest dst cp (Just v) = Just (v {lowest = Just dst, lowestDirection = cp})

visiblityWithRvr :: RunwayDesignator -> VisibilityDistance -> Maybe ExtremeRvr -> Maybe VisibilityTendency -> Maybe Visibility -> Maybe Visibility
visiblityWithRvr rwy dst ext tdc Nothing = Just (Visibility (VisibilityDistanceMetre 0) Nothing Nothing [RunwayVisualRange rwy dst ext tdc])
visiblityWithRvr rwy dst ext tdc (Just v) = Just (v {runways = RunwayVisualRange rwy dst ext tdc : runways v})

-- ----------------
-- Private parsers.
-- ----------------
-- | 'WindDirection' parser.
wdParser :: Parser WindDirection
wdParser = natural 3 >>= mkWindDirection

-- | 'VariableDirection' parser.
variableDirectionParser :: Parser WindVariableDirection
variableDirectionParser = do
    _ <- space
    l <- wdParser
    _ <- char 'V'
    r <- wdParser
    return (WindVariableDirection l r)

-- | if VRB -> Nothing, else parseDegrees.
windDirectionParser :: Parser (Maybe WindDirection)
windDirectionParser = do
    var <- fmap isJust (optional (string "VRB"))
    if var
        then return Nothing
        else fmap Just wdParser

-- | 'Calm' parser.
calmParser :: Parser (Maybe Wind)
calmParser = do
    _ <- string "00000"
    _ <- enumeration :: Parser SpeedUnit
    return Nothing

-- | 'Wind' parser.
-- wind direction on 3 digits, degrees
-- speed on 2 digits
-- optionally gust speed on 2 digits
-- speed unit.
windParser :: Parser (Maybe Wind)
windParser = do
    d <- windDirectionParser
    s <- natural 2
    g <- optional (char 'G' >> natural 2)
    u <- enumeration :: Parser SpeedUnit
    v <- optional (try variableDirectionParser)
    return (Just (Wind d (speedFrom s u) (fmap (`speedFrom` u) g) v))

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
    d <-
        case u of
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
    -- fraction can start with a space if unit of mile is not present
    _ <- optional space
    n <- natural 1
    _ <- slash
    d <- natural 1
    _ <- string "SM"
    return (n, d)

-- | FAA visibility: only prevailing visibility in miles and fraction ending with SM
-- at least mile (1 digit) or fraction (1 digit / 1 digit) present.
faaVisibilityParser :: Parser Visibility
faaVisibilityParser = do
    m <- optional (try ((natural 2 <|> natural 1) <* (string "SM" <|> string " ")))
    f <- optional (try mileFractionParser)
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
