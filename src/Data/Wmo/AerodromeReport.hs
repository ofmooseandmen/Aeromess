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
-- - <https://en.wikipedia.org/wiki/Metar METAR/SPECI on Wikipedia>
-- - <http://sto.iki.fi/metar>
--
-- TODO: break module when implementing TAF
-- TODO: support trend
--
module Data.Wmo.AerodromeReport
    ( module Data.Icao.Location
    , module Data.Icao.Time
    -- * Data
    , ReportType(..)
    , SpeedUnit(..)
    , WindVariableDirection(wvdLeft, wvdRight)
    , WindSpeed(wsUnit, wsValue)
    , Wind(wDirection, wSpeed, wGust, wVariation)
    , VisibilityTendency(..)
    , VisibilityDistance(vdUnit, vdValue, vdFraction)
    , RunwayDesignator
    , ExtremeRvr(..)
    , RunwayVisualRange(rvrDesignator, rvrMeanVisibility,
                  rvrOutsideMeasuringRange, rvrVisibilityTendency)
    , CompassPoint(..)
    , Visibility(vPrevailing, vLowest, vLowestDirection, vRunways)
    , WeatherQualifier(..)
    , WeatherDescriptor(..)
    , WeatherPhenomenon(..)
    , Weather(wQualifier, wDescriptor, wPhenomenon)
    , CloudType(..)
    , CloudAmountType(..)
    , CloudAmount(caHeight, caType, clType)
    , VerticalVisibility(vvExtent)
    , NoCloudObserved(..)
    , Clouds(..)
    , Pressure(pUnit, pValue)
    , ReportModifiers(reportCorrected, reportAuto, reportMissed)
    , AerodromeReport(reportType, reportStation, reportDate, reportWind,
                reportVisibility, reportWeather, reportClouds, reportTemperature,
                reportDewPoint, reportPressure, reportRemarks)
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
    , withWeather
    , withCloudAmount
    , withObscuredSky
    , withNoCloudObserved
    , withTemperature
    , withPressure
    , withFaaPressure
    -- * Parsers
    , Parser
    , Error(message, column)
    , parser
    , parse
    ) where

import Control.Monad ((<=<))
import Control.Monad.Fail
import Data.Aeromess.Parser
import Data.Char (isDigit)
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time
import Data.Maybe
import Prelude hiding (fail)

-- | type of report.
data ReportType
    = METAR -- ^ periodic or routine report, e.g. generated once an hour or half hour.
    | SPECI -- ^ special report issued when conditions have significantly changed.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Speed unit.
-- TODO: move to Icao?
data SpeedUnit
    = KT -- ^ knots.
    | MPS -- ^ metres per second.
    | KMH -- ^ kilometres per hour.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Distance unit.
-- Note: unit abbreviation is not used to avoid conflicts.
-- TODO: move to Icao?
data LengthUnit
    = Metre -- ^ metre, standard unit.
    | Foot -- ^ foot, used by FAA for RVR.
    | Mile -- ^ mile, more formally statute mile, used by FAA.
    deriving (Eq, Show)

-- | Variable wind direction.
data WindVariableDirection = WindVariableDirection
    { wvdLeft :: Int -- ^ the left extreme of the wind direction in degrees.
    , wvdRight :: Int -- ^ the right extreme of the wind direction in degrees.
    } deriving (Eq, Show)

-- | Wind speed in appropriate unit.
data WindSpeed = WindSpeed
    { wsUnit :: SpeedUnit -- ^ wind speed unit.
    , wsValue :: Int -- ^ value of the wind speed in the unit.
    } deriving (Eq, Show)

-- | Wind group data.
-- direction `000` and speed `00` indicates calm conditions.
data Wind = Wind
    { wDirection :: Maybe Int -- ^ mean true direction in degrees rounded off to the nearest 10 degrees
                              -- from which the wind is blowing, when absent the direction is variable.
    , wSpeed :: WindSpeed -- ^ mean speed of the wind over the 10-minute period immediately preceding the observation.
    , wGust :: Maybe WindSpeed -- ^ maximum gust wind speed if relevant.
    , wVariation :: Maybe WindVariableDirection -- ^ variable wind direction if relevant.
    } deriving (Eq, Show)

-- | Visibility tendency at the runway.
data VisibilityTendency
    = Down
    | Up
    | NoChange
    deriving (Eq, Show)

-- | Visibility distance in appropriate unit.
data VisibilityDistance = VisibilityDistance
    { vdUnit :: LengthUnit -- ^ distance unit
    , vdValue :: Int -- ^ distance value in the unit.
    , vdFraction :: Maybe (Int, Int) -- ^ distance fraction in the unit, e.g. (1, 4) for a 1/4.
    } deriving (Eq, Show)

-- | Runway designator: 2 digits possibly appended with L(eft) C(entral) or R(ight) for parallel runways.
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
    { rvrDesignator :: RunwayDesignator -- ^ runway designator.
    , rvrMeanVisibility :: VisibilityDistance -- ^ mean visibility in metre.
    , rvrOutsideMeasuringRange :: Maybe ExtremeRvr -- ^ present only if the RVR values are outside the measuring range of the observing system.
    , rvrVisibilityTendency :: Maybe VisibilityTendency -- ^ visibility tendency.
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
    { vPrevailing :: VisibilityDistance -- ^ prevailing horizontal visibility in metres, 9999 indicates a visibility over 10 km.
    , vLowest :: Maybe VisibilityDistance -- ^ lowest visibility if reported.
    , vLowestDirection :: Maybe CompassPoint -- ^ lowest visibility.
    , vRunways :: [RunwayVisualRange] -- ^ visual range for each runway.
    } deriving (Eq, Show)

-- | Weather qualifier.
data WeatherQualifier
    = LightWeather -- ^ light weather.
    | HeavyWeather -- ^ heavy weather.
    | InVicinityWeather -- in vicinity, i.e. not on location but within 8000 m.
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
    { wQualifier :: Maybe WeatherQualifier -- ^ qualifier, when absent denotes moderate weather.
    , wDescriptor :: Maybe WeatherDescriptor -- ^ descriptor, if applicable.
    , wPhenomenon :: [WeatherPhenomenon] -- ^phenomenon(s).
    } deriving (Eq, Show)

-- | Cloud type.
data CloudType
    = Cumulonimbus
    | ToweringCumulus
    deriving (Eq, Show)

-- | Cloud amount type.
data CloudAmountType
    = Few -- ^ few amount of clouds.
    | Scattered -- ^ scattered amount of clouds.
    | Broken -- ^ broken amount of clouds.
    | Overcast -- ^ overcast amount of clouds.
    deriving (Eq, Show)

-- | Amount of clouds.
data CloudAmount = CloudAmount
    { caType :: CloudAmountType
    , caHeight :: Maybe Int -- ^ height of cloud base in hundreds of feet.
    , clType :: Maybe CloudType -- ^ type of clouds if relevant.
    } deriving (Eq, Show)

-- | Vertical visibility in hundreds of feet.
newtype VerticalVisibility = VerticalVisibility
    { vvExtent :: Maybe Int -- ^ extent of vertical visibility in hundreds of feet above field elevation.
    } deriving (Eq, Show)

-- | Sky conditions when cloud were not observed.
data NoCloudObserved
    = NoCloudBelow1500
    | NoCloudBelow3600
    | SkyClear
    deriving (Eq, Show)

-- | Clouds observation.
data Clouds
    = CloudAmounts [CloudAmount]
    | ObscuredSky VerticalVisibility
    | NoneObserved NoCloudObserved
    deriving (Eq, Show)

-- | Mean sea level pressure unit.
data PressureUnit
    = Hpa -- ^ hPa (standard).
    | InchesHg -- ^ inches of mercury (US).
    deriving (Enum, Eq, Show)

-- | Mean sea level pressure.
data Pressure = Pressure
    { pUnit :: PressureUnit -- ^ pressure unit.
    , pValue :: Int -- ^ pressure value in the unit.
    } deriving (Eq, Show)

-- | Report modifiers.
data ReportModifiers = ReportModifiers
    { reportCorrected :: Bool -- ^ whether the report was corrected.
    , reportAuto :: Bool -- ^ whether the report contains fully automated observations without human intervention.
    , reportMissed :: Bool -- ^ whether the report corresponds to a missing report.
    } deriving (Eq, Show)

-- | an aerodrome routine or special meteorological report.
data AerodromeReport = AerodromeReport
    { reportType :: ReportType -- ^ type of the report.
    , reportModifiers :: ReportModifiers -- ^ modifiers (cor, auto, nil).
    , reportStation :: Aerodrome -- ^ ICAO code for the observing station (an aerodrome).
    , reportDate :: DayTime -- ^ when the observation was made.
    , reportWind :: Maybe Wind -- ^ wind related observations.
    , reportVisibility :: Maybe Visibility -- ^ visibility related observations.
    , reportWeather :: [Weather] -- ^ weather related observations.
    , reportClouds :: Maybe Clouds -- ^ clouds related observations.
    , reportTemperature :: Int -- ^ temperature rounded to nearest whole degree Celsius.
    , reportDewPoint :: Int -- ^ dew point rounded to nearest whole degree Celsius.
    , reportPressure :: Pressure -- ^ mean sea level pressure (“QNH”).
    , reportRemarks :: Maybe FreeText -- ^ Report components and miscellaneous abbreviations.
    } deriving (Eq, Show)

----------------
-- Private data.
----------------
-- | 'CompassPoint' code, only defined to facilitate parsing.
data CompassPointCode
    = NE
    | SE
    | SW
    | NW
    | N
    | E
    | S
    | W
    deriving (Bounded, Enum, Eq, Read, Show)

-- | 'WeatherDescriptor' code, only defined to facilitate parsing.
data WeatherDescriptorCode
    = MI
    | BC
    | PR
    | DR
    | BL
    | SH
    | TS
    | FZ
    deriving (Bounded, Enum, Eq, Read, Show)

-- | 'WeatherPhenomenon' code, only defined to facilitate parsing.
data WeatherPhenomenonCode
    = DZ
    | RA
    | SN
    | SG
    | IC
    | PL
    | GR
    | GS
    | UP
    | BR
    | FG
    | FU
    | VA
    | DU
    | SA
    | HZ
    | PY
    | PO
    | SQ
    | FC
    | SS
    | DS
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Determines the given 'AerodromeReport' reports Cloud And Visibility OK (CAVOK).
-- CAVOK is an abbreviation for Cloud And Visibility OK, indicating no cloud below 5,000 ft
-- (1,500 m) or the highest minimum sector altitude and no cumulonimbus or towering cumulus
-- at any level, a visibility of 10 km (6 mi) or more and no significant weather change.
cavok :: AerodromeReport -> Bool
cavok m = isNothing (reportVisibility m) && null (reportWeather m) && isNothing (reportClouds m)

-- | Builds a periodic 'AerodromeReport' (METAR) for the given station and time and all given
-- setter.
--
-- If no @setters@ are provided, the METAR will contain no Wind (Calm conditions),
-- report 'cavok' conditions ISA temperature (15 degrees Celsius) and pressure (1013 hPa)
--
-- See 'with' and @withXXXX@ functions.
--
-- >  example =
-- >    Report "ESSA" (18, 21, 48) [withWindDirection 150, withWindSpeed 40 Nothing KT]
--
metar
    :: (MonadFail m)
    => String -> (Int, Int, Int) -> [AerodromeReport -> m AerodromeReport] -> m AerodromeReport
metar st dt setters = defaultReport METAR st dt >>= with setters

-- | Builds a special 'AerodromeReport' (SPECI) for the given station and time and all given
-- setter.
-- see 'metar'.
speci
    :: (MonadFail m)
    => String -> (Int, Int, Int) -> [AerodromeReport -> m AerodromeReport] -> m AerodromeReport
speci st dt setters = defaultReport SPECI st dt >>= with setters

-- | Modifies the given @report@ by running each given setter.
with
    :: (MonadFail m)
    => [AerodromeReport -> m AerodromeReport] -> AerodromeReport -> m AerodromeReport
with = foldl (<=<) return

-- | Modifies the given @report@ by setting the modifiers (COR, AUTO, MIS).
withModifiers
    :: (MonadFail m)
    => (Bool, Bool, Bool) -> AerodromeReport -> m AerodromeReport
withModifiers modifs report = do
    let (cor, aut, mis) = modifs
    return report {reportModifiers = ReportModifiers cor aut mis}

-- | Modifies the given @report@ by setting the wind direction.
withWindDirection
    :: (MonadFail m)
    => Int -> AerodromeReport -> m AerodromeReport
withWindDirection dir report = do
    _dir <- mkWindDirection dir
    return report {reportWind = windWithDirection _dir (reportWind report)}

-- | Modifies the given @report@ by setting the wind speed (@spd@) and gust (@gst).
withWindSpeed
    :: (MonadFail m)
    => Int -> Maybe Int -> SpeedUnit -> AerodromeReport -> m AerodromeReport
withWindSpeed spd gst ut report = do
    _spd <- mkWindSpeed spd ut
    _gst <-
        case gst of
            Nothing -> return Nothing
            Just s -> fmap Just (mkWindSpeed s ut)
    return report {reportWind = windWithSpeed _spd _gst (reportWind report)}

-- | Modifies the given @report@ by setting the wind variation.
withWindVariation
    :: (MonadFail m)
    => Int -> Int -> AerodromeReport -> m AerodromeReport
withWindVariation lft rgt report = do
    _lft <- mkWindDirection lft
    _rgt <- mkWindDirection rgt
    return
        report
        {reportWind = windWithVariation (WindVariableDirection _lft _rgt) (reportWind report)}

-- | Modifies the given @report@ by setting the prevailing visibility according to the WMO standard.
-- the visibility is expressed in Metres.
withPrevailingVisibility
    :: (MonadFail m)
    => Int -> AerodromeReport -> m AerodromeReport
withPrevailingVisibility dst report = do
    _dst <- mkVisibilityDistanceMetre dst
    return report {reportVisibility = visiblityWithPrevailing _dst (reportVisibility report)}

-- | Modifies the given @report@ by setting the prevailing visibility according to the FAA standard.
-- the visibility is expressed in statue miles (@mile@ and @fraction@).
withFaaPrevailingVisibility
    :: (MonadFail m)
    => Maybe Int -> Maybe (Int, Int) -> AerodromeReport -> m AerodromeReport
withFaaPrevailingVisibility m f report = do
    _dst <- mkVisibilityDistanceMile m f
    return report {reportVisibility = visiblityWithPrevailing _dst (reportVisibility report)}

-- | Modifies the given @report@ by setting the lowest visibility (Metre) and for a direction (if any).
-- Note: this is not supported by the FAA standard.
withLowestVisibility
    :: (MonadFail m)
    => Int -> Maybe CompassPoint -> AerodromeReport -> m AerodromeReport
withLowestVisibility dst cp report = do
    _dst <- mkVisibilityDistanceMetre dst
    return report {reportVisibility = visiblityWithLowest _dst cp (reportVisibility report)}

-- | Modifies the given @report@ by adding a runway visual range (RVR) according to the WMO standard.
-- the runway visibility is expressed in Metres.
withRunwayVisualRange
    :: (MonadFail m)
    => String
    -> Int
    -> Maybe ExtremeRvr
    -> Maybe VisibilityTendency
    -> AerodromeReport
    -> m AerodromeReport
withRunwayVisualRange rwy dst ext tdc report = do
    _rwy <- mkRunwayDesignator rwy
    _dst <- mkVisibilityDistanceMetre dst
    return
        report
        { reportVisibility =
              visiblityWithRvr (RunwayVisualRange _rwy _dst ext tdc) (reportVisibility report)
        }

-- | Modifies the given @report@ by adding a runway visual range (RVR) according to the FAA standard.
-- the runway visibility is expressed in feet.
withFaaRunwayVisualRange
    :: (MonadFail m)
    => String
    -> Int
    -> Maybe ExtremeRvr
    -> Maybe VisibilityTendency
    -> AerodromeReport
    -> m AerodromeReport
withFaaRunwayVisualRange rwy dst ext tdc report = do
    _rwy <- mkRunwayDesignator rwy
    _dst <- mkVisibilityDistanceFeet dst
    return
        report
        { reportVisibility =
              visiblityWithRvr (RunwayVisualRange _rwy _dst ext tdc) (reportVisibility report)
        }

-- | Modifies the given @report@ by adding a weather observation.
withWeather
    :: (MonadFail m)
    => Maybe WeatherQualifier
    -> Maybe WeatherDescriptor
    -> [WeatherPhenomenon]
    -> AerodromeReport
    -> m AerodromeReport
withWeather q d p report = return v
  where
    v = reportWithWeather (Weather q d p) report

-- | Modifies the given @report@ by adding the cloud amount observation.
withCloudAmount
    :: (MonadFail m)
    => CloudAmountType -> Maybe Int -> Maybe CloudType -> AerodromeReport -> m AerodromeReport
withCloudAmount cat h ct report = do
    ca <- mkCloudAmount cat h ct
    return (reportWithCloudAmount ca report)

-- | Modifies the given @report@ by setting the obscured sky observation.
withObscuredSky
    :: (MonadFail m)
    => Maybe Int -> AerodromeReport -> m AerodromeReport
withObscuredSky h report = do
    os <- mkObscuredSky h
    return (reportWithSkyCondition os report)

-- | Modifies the given @report@ by setting the reason why no clouds were observed.
withNoCloudObserved
    :: (MonadFail m)
    => NoCloudObserved -> AerodromeReport -> m AerodromeReport
withNoCloudObserved nco report = return (reportWithSkyCondition (NoneObserved nco) report)

-- | Modifies the given @report@ by setting the temperature and dew point.
withTemperature
    :: (MonadFail m)
    => Int -> Int -> AerodromeReport -> m AerodromeReport
withTemperature t d report = do
    _t <- mkTemperature t
    _d <- mkTemperature d
    return report {reportTemperature = _t, reportDewPoint = _d}

-- | Modifies the given @report@ by setting the pressure in Hectopascals.
withPressure
    :: (MonadFail m)
    => Int -> AerodromeReport -> m AerodromeReport
withPressure p report = do
    _p <- mkPressure p Hpa
    return report {reportPressure = _p}

-- | Modifies the given @report@ by setting the pressure in inches of mercury.
withFaaPressure
    :: (MonadFail m)
    => Int -> AerodromeReport -> m AerodromeReport
withFaaPressure p report = do
    _p <- mkPressure p InchesHg
    return report {reportPressure = _p}

-- | 'AerodromeReport' parser.
-- This parser supports both the WMO code definition and the variation defined by the FAA.
parser :: Parser AerodromeReport
parser = do
    rt <- enumeration :: Parser ReportType
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
    t <- temperatureParser
    _ <- slash
    d <- temperatureParser
    _ <- space
    p <- pressureParser
    let (vs, we, cl) = fromMaybe (Nothing, [], Nothing) vwc
    let m = ReportModifiers (cor1 || cor2) au ms
    -- TODO, once everything is parsed, check for '=' or end of line.
    return (AerodromeReport rt m st dt wd vs we cl t d p Nothing)

-- | Parses the given textual representation of a 'AerodromeReport' using 'Parser'.
-- return either an 'Error' ('Left') or the parsed 'AerodromeReport' ('Right').
parse :: String -> Either Error AerodromeReport
parse = runParser parser

-- ---------------------------
-- Private smart constructors.
-- ---------------------------
noModifiers :: ReportModifiers
noModifiers = ReportModifiers False False False

mkRunwayDesignator
    :: (MonadFail m)
    => String -> m RunwayDesignator
mkRunwayDesignator s
    | length s /= 2 && length s /= 3 = fail ("invalid runway designator=" ++ s)
    | not (all isDigit (take 2 s)) = fail ("invalid runway designator=" ++ s)
    | length s == 3 && (last s /= 'C' && last s /= 'R' && last s /= 'L') =
        fail ("invalid runway designator=" ++ s)
    | otherwise = return (RunwayDesignator s)

mkWindSpeed
    :: (MonadFail m)
    => Int -> SpeedUnit -> m WindSpeed
mkWindSpeed s u
    | s < 0 || s > 99 = fail ("invalid wind speed [" ++ show u ++ "]=" ++ show s)
    | otherwise = return (WindSpeed u s)

mkWindDirection
    :: (MonadFail m)
    => Int -> m Int
mkWindDirection d
    | d < 0 || d > 359 = fail ("invalid wind direction [degrees]=" ++ show d)
    | otherwise = return d

mkVisibilityDistanceMetre
    :: (MonadFail m)
    => Int -> m VisibilityDistance
mkVisibilityDistanceMetre m
    | m < 0 || m > 9999 = fail ("invalid visibility distance [metre]=" ++ show m)
    | otherwise = return (VisibilityDistance Metre m Nothing)

mkVisibilityDistanceMile
    :: (MonadFail m)
    => Maybe Int -> Maybe (Int, Int) -> m VisibilityDistance
mkVisibilityDistanceMile Nothing Nothing = fail "invalid visibility distance [mile]"
mkVisibilityDistanceMile m f
    | maybe False (< 0) m || maybe False (> 99) m =
        fail ("invalid visibility distance [mile]=" ++ show m)
    | maybe False (< 0) (fmap fst f) || maybe False (> 9) (fmap fst f) =
        fail ("invalid visibility distance [fraction]=" ++ show f)
    | maybe False (< 0) (fmap snd f) || maybe False (> 9) (fmap snd f) =
        fail ("invalid visibility distance [fraction]=" ++ show f)
    | otherwise = return (VisibilityDistance Mile (fromMaybe 0 m) f)

mkVisibilityDistanceFeet
    :: (MonadFail m)
    => Int -> m VisibilityDistance
mkVisibilityDistanceFeet m
    | m < 0 || m > 9999 = fail ("invalid visibility distance [feet]=" ++ show m)
    | otherwise = return (VisibilityDistance Foot m Nothing)

mkCloudAmount
    :: (MonadFail m)
    => CloudAmountType -> Maybe Int -> Maybe CloudType -> m CloudAmount
mkCloudAmount ca Nothing ct = return (CloudAmount ca Nothing ct)
mkCloudAmount ca (Just ft) ct
    | ft < 0 || ft > 999 = fail ("invalid cloud amount height [hundreds feet]=" ++ show ft)
    | otherwise = return (CloudAmount ca (Just ft) ct)

mkObscuredSky
    :: (MonadFail m)
    => Maybe Int -> m Clouds
mkObscuredSky Nothing = return (ObscuredSky (VerticalVisibility Nothing))
mkObscuredSky (Just ft)
    | ft < 0 || ft > 999 = fail ("invalid vertical visibility [hundreds feet]=" ++ show ft)
    | otherwise = return (ObscuredSky (VerticalVisibility (Just ft)))

mkTemperature :: (MonadFail m) => Int -> m Int
mkTemperature t
    | t < -99 || t > 99 = fail ("invalid temperature [celsius]=" ++ show t)
    | otherwise = return t

mkPressure:: (MonadFail m) => Int -> PressureUnit -> m Pressure
mkPressure p u
    | p < 0 || p > 9999 = fail ("invalid pressure [" ++ show u ++ "]=" ++ show p)
    | otherwise = return (Pressure u p)

-- ------------------------
-- Private builder helpers.
-- -------------------------

isaTemperature :: Int
isaTemperature = 15

isaPression :: Pressure
isaPression = Pressure Hpa 1013

-- | default 'AerodromeReport' of given type for given station and time
-- No wind, and CAVOK conditions.
-- TODO: ISA conditions (pressure and temperature)
defaultReport
    :: (MonadFail m)
    => ReportType -> String -> (Int, Int, Int) -> m AerodromeReport
defaultReport t st (d, h, m) = do
    _st <- mkAerodrome st
    _dt <- mkDayTime d h m
    return
        (AerodromeReport
             t
             noModifiers
             _st
             _dt
             Nothing
             Nothing
             []
             Nothing
             isaTemperature
             isaTemperature
             isaPression
             Nothing)

windWithDirection :: Int -> Maybe Wind -> Maybe Wind
windWithDirection dir Nothing = Just (Wind (Just dir) (WindSpeed KT 0) Nothing Nothing)
windWithDirection dir (Just wd) = Just (wd {wDirection = Just dir})

windWithSpeed :: WindSpeed -> Maybe WindSpeed -> Maybe Wind -> Maybe Wind
windWithSpeed spd gst Nothing = Just (Wind Nothing spd gst Nothing)
windWithSpeed spd gst (Just wd) = Just (wd {wSpeed = spd, wGust = gst})

windWithVariation :: WindVariableDirection -> Maybe Wind -> Maybe Wind
windWithVariation var Nothing = Just (Wind Nothing (WindSpeed KT 0) Nothing (Just var))
windWithVariation var (Just wd) = Just (wd {wVariation = Just var})

visiblityWithPrevailing :: VisibilityDistance -> Maybe Visibility -> Maybe Visibility
visiblityWithPrevailing dst Nothing = Just (Visibility dst Nothing Nothing [])
visiblityWithPrevailing dst (Just v) = Just (v {vPrevailing = dst})

visiblityWithLowest :: VisibilityDistance
                    -> Maybe CompassPoint
                    -> Maybe Visibility
                    -> Maybe Visibility
visiblityWithLowest dst cp Nothing =
    Just (Visibility (VisibilityDistance Metre 0 Nothing) (Just dst) cp [])
visiblityWithLowest dst cp (Just v) = Just (v {vLowest = Just dst, vLowestDirection = cp})

visiblityWithRvr :: RunwayVisualRange -> Maybe Visibility -> Maybe Visibility
visiblityWithRvr rvr Nothing =
    Just (Visibility (VisibilityDistance Metre 0 Nothing) Nothing Nothing [rvr])
visiblityWithRvr rvr (Just v) = Just (v {vRunways = rvr : vRunways v})

reportWithWeather :: Weather -> AerodromeReport -> AerodromeReport
reportWithWeather w r = r {reportWeather = w : reportWeather r}

reportWithCloudAmount :: CloudAmount -> AerodromeReport -> AerodromeReport
reportWithCloudAmount ca r =
    case reportClouds r of
        (Just (CloudAmounts cur)) -> r {reportClouds = Just (CloudAmounts (ca : cur))}
        _ -> r {reportClouds = Just (CloudAmounts [ca])}

reportWithSkyCondition :: Clouds -> AerodromeReport -> AerodromeReport
reportWithSkyCondition sc r = r {reportClouds = Just sc}

-- ----------------
-- Private parsers.
-- ----------------
-- | wind direction parser.
wdParser :: Parser Int
wdParser = natural 3 >>= mkWindDirection

-- | 'WindVariableDirection' parser.
variableDirectionParser :: Parser WindVariableDirection
variableDirectionParser = do
    _ <- space
    l <- wdParser
    _ <- char 'V'
    r <- wdParser
    return (WindVariableDirection l r)

-- | if VRB -> Nothing, else @wdParser@.
windDirectionParser :: Parser (Maybe Int)
windDirectionParser = do
    var <- fmap isJust (optional (string "VRB"))
    if var
        then return Nothing
        else fmap Just wdParser

-- | 'Calm' parser.
calmParser :: Parser (Maybe Wind)
calmParser = do
    _ <- try (string "00000")
    _ <- enumeration :: Parser SpeedUnit
    return Nothing

-- | 'Wind' parser.
-- wind direction on 3 digits, degrees
-- speed on 2 digits
-- optionally gust speed on 2 digits
-- speed unit
windParser :: Parser (Maybe Wind)
windParser = do
    d <- windDirectionParser
    s <- natural 2
    g <- optional (char 'G' >> natural 2)
    u <- enumeration :: Parser SpeedUnit
    v <- optional (try variableDirectionParser)
    return (Just (Wind d (WindSpeed u s) (fmap (WindSpeed u) g) v))

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
    -- FAA specifies mean visibility in feet, if nothing, it's Metres.
    u <- optional (string "FT")
    d <-
        case u of
            Just _ -> return (VisibilityDistance Foot _d Nothing)
            _ -> return (VisibilityDistance Metre _d Nothing)
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
wmoVisibilityParser :: Parser (VisibilityDistance, Maybe VisibilityDistance, Maybe CompassPoint)
wmoVisibilityParser = do
    v <- natural 4
    _ <- space
    l <- optional (try (natural 4))
    d <-
        case l of
            Nothing -> return Nothing
            Just _ -> fmap Just (compassPointParser <* space)
    return
        (VisibilityDistance Metre v Nothing, fmap (\lv -> VisibilityDistance Metre lv Nothing) l, d)

-- | 'x/ySM' parser.
mileFractionParser :: Parser (Int, Int)
mileFractionParser = do
    _ <- optional space -- fraction can start with a space if unit of mile is not present
    n <- natural 1
    _ <- slash
    d <- natural 1
    _ <- string "SM"
    return (n, d)

-- | FAA visibility: only prevailing visibility in miles and fraction ending with SM
-- at least mile (1 digit) or fraction (1 digit / 1 digit) present.
faaVisibilityParser :: Parser (VisibilityDistance, Maybe VisibilityDistance, Maybe CompassPoint)
faaVisibilityParser = do
    unitOnly <- optional (try ((natural 2 <|> natural 1) <* string "SM"))
    dm <-
        case unitOnly of
            Just _ -> mkVisibilityDistanceMile unitOnly Nothing
            Nothing -> do
                u <- optional (try ((natural 2 <|> natural 1) <* string " "))
                f <- mileFractionParser
                mkVisibilityDistanceMile u (Just f)
    _ <- space
    return (dm, Nothing, Nothing)

-- | 'Visibility' parser.
-- This parse is called only when the report does not contain CAVOK
-- in which case visibility is required.
-- FAA deviates from the WMO standard here.
visibilityParser :: Parser Visibility
visibilityParser = do
    (v, l, d) <- try wmoVisibilityParser <|> faaVisibilityParser
    r <- try rvrsParser
    -- depending on what was successfully parsed (e.g. 9999) there maybe a space before the next field...
    _ <- optional space
    return (Visibility v l d r)

wQualifierParser :: Parser WeatherQualifier
wQualifierParser = do
    q <- choice [string "-", string "+", string "VC"]
    case q of
        "-" -> return LightWeather
        "+" -> return HeavyWeather
        _ -> return InVicinityWeather

wDescriptorParser :: Parser WeatherDescriptor
wDescriptorParser = do
    d <- enumeration :: Parser WeatherDescriptorCode
    case d of
        MI -> return Shallow
        BC -> return Patches
        PR -> return Partial
        DR -> return Drifting
        BL -> return Blowing
        SH -> return Showers
        TS -> return Thunderstorm
        FZ -> return Freezing

wPhenomenonParser :: Parser WeatherPhenomenon
wPhenomenonParser = do
    p <- enumeration :: Parser WeatherPhenomenonCode
    case p of
        DZ -> return Drizzle
        RA -> return Rain
        SN -> return Snow
        SG -> return SnowGrains
        IC -> return IceCrystals
        PL -> return IcePellets
        GR -> return Hail
        GS -> return SmallHail
        UP -> return UnknownPrecipitation
        BR -> return Mist
        FG -> return Fog
        FU -> return Smoke
        VA -> return VolcanicAsh
        DU -> return WidespreadDust
        SA -> return Sand
        HZ -> return Haze
        PY -> return Spray
        PO -> return Dust
        SQ -> return Squall
        FC -> return FunnelCloud
        SS -> return Sandstorm
        DS -> return DustStorm

-- | 'Weather' parser.
weatherParser :: Parser Weather
weatherParser = do
    q <- optional wQualifierParser
    d <- optional wDescriptorParser
    p <- manyTillSpace wPhenomenonParser
    return (Weather q d p)

-- | parser of a list of 'Weather'.
weathersParser :: Parser [Weather]
weathersParser = many (try weatherParser)

clearishSkyParser :: Parser (Maybe Clouds)
clearishSkyParser = do
    key <- choice [try (string "NSC"), string "NCD", string "CLR", string "SKC"]
    _ <- space
    case key of
        "NSC" -> return (Just (NoneObserved NoCloudBelow1500))
        "NCD" -> return (Just (NoneObserved SkyClear))
        "CLR" -> return (Just (NoneObserved NoCloudBelow3600))
        "SKC" -> return (Just (NoneObserved SkyClear))
        _ -> unexpected "cloud key"

heightParser :: Parser (Maybe Int)
heightParser = do
    h <- optional (natural 3)
    case h of
        Nothing -> do
            _ <- string "///"
            return Nothing
        Just _ -> return h

cloudAmountParser :: Parser CloudAmount
cloudAmountParser = do
    catk <- choice [string "FEW", string "SCT", string "BKN", string "OVC"]
    cat <-
        case catk of
            "FEW" -> return Few
            "SCT" -> return Scattered
            "BKN" -> return Broken
            "OVC" -> return Overcast
            _ -> unexpected "Cloud amount"
    vv <- heightParser
    ctk <- optional (choice [string "CB", string "TBU"])
    ct <-
        case ctk of
            Just "CB" -> return (Just Cumulonimbus)
            Just "TCU" -> return (Just ToweringCumulus)
            _ -> return Nothing
    ca <- mkCloudAmount cat vv ct
    _ <- space
    return ca

cloudAmountsParser :: Parser (Maybe Clouds)
cloudAmountsParser = do
    cas <- some cloudAmountParser
    return (Just (CloudAmounts cas))

verticalVisibilityParser :: Parser (Maybe Clouds)
verticalVisibilityParser = do
    _ <- string "VV"
    vv <- heightParser
    os <- mkObscuredSky vv
    _ <- space
    return (Just os)

-- | parser of a list of 'Clouds'
-- The following keywords are defined by the WMO
-- - NSC -> NoCloudBelow1500
-- - NCD -> SkyClear
-- The following keywords are defined by the FAA
-- - CLR -> NoCloudBelow3600 (12000 ft)
-- The following keywords seems to be also used
-- - SKC -> SkyClear
--
-- heights is coded in hundreds of feet on 3 digits
--
cloudsParser :: Parser (Maybe Clouds)
cloudsParser =
    choice [clearishSkyParser, cloudAmountsParser, verticalVisibilityParser, return Nothing]

-- | CAVOK or visibility, weather and clouds parser.
-- returns nothing if CAVOK
vwcParser :: Parser (Maybe (Maybe Visibility, [Weather], Maybe Clouds))
vwcParser = do
    ok <- fmap isJust (optional (string "CAVOK "))
    if ok
        then return Nothing
        else do
            vs <- visibilityParser
            we <- weathersParser
            cs <- cloudsParser
            return (Just (Just vs, we, cs))

temperatureParser :: Parser Int
temperatureParser = do
    neg <- optional (char 'M')
    t <- natural 2
    if isJust neg then
        mkTemperature (negate t)
    else
        mkTemperature t

pressureParser :: Parser Pressure
pressureParser = do
    u <- char 'A' <|> char 'Q'
    p <- natural 4
    case u of
        'A' -> mkPressure p InchesHg
        'Q' -> mkPressure p Hpa
        _   -> unexpected "pression"
