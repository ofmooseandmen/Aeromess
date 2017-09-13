-- | This module provides tools to parse and format METAR.
-- METAR is a format for reporting weather information.
-- This module is compliant with WMO Technical Regulations - Annex II, but also
-- best effort have been made to cater for small deviations (mostly for US and Canadian METAR).
-- see <http://www.wmo.int/pages/prog/www/WMOCodes/WMO306_vI1/Publications/2016update/WMO306_vI1_en_2011UP2016.pdf>
-- see <https://en.wikipedia.org/wiki/METAR>
-- see <http://sto.iki.fi/metar>
-- see <http://weather.unisys.com/wxp/Appendices/Formats/METAR.html>
module Data.Wmo.Metar where

import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time

-- | type of report.
data Type
    = METAR -- ^ periodic report, e.g. generated once an hour or half hour
    | SPECI -- ^ special report issued when conditions have significantly changed
    deriving (Eq, Show)

-- | date and time.
data DayTime = DayTime
    { day :: Int -- ^ day of month
    , time :: Hhmm -- ^ hour minute
    } deriving (Eq, Show)

-- | Speed unit.
data SpeedUnit
    = KT -- ^ knots
    | MPS -- meters per second
    deriving (Eq, Show)

-- | Variable wind direction.
data VariableDirection = VariableDirection
    { left :: Int -- ^ the left extreme of wind direction
    , right :: Int -- ^ the right extreme of wind direction
    } deriving (Eq, Show)

-- | Wind group data.
-- direction `000` and speed `00` indicates calm conditions
data Wind = Wind
    { direction :: Int -- ^ mean true direction in degrees rounded off to the nearest 10 degrees from which the wind is blowing
    , speed :: Int -- ^ mean speed of the wind over the 10-minute period immediately preceding the observation
    , gust :: Maybe Int -- ^ maximum gust wind speed if relevant
    , speedUnit :: SpeedUnit -- ^ speed unit
    , variation :: Maybe VariableDirection -- ^ variable wind direction if relevant
    } deriving (Eq, Show)

-- | Runway visibility data.
data RunwayVisibility = RunwayVisibility
    { designator :: String -- ^ runway designator, possibility appended with L(eft) C(entral) or R(ight) for parallel runways.
    , minVisibility :: Int -- ^ visibility in meter or minimum visibility if it varies significantly
    , maxVisibility :: Maybe Int -- ^ maximum visibility in meter, if visibility varies significantly
    } deriving (Eq, Show)

-- | Visibility group data.
data Visibility = Visibility
    { horizontal :: Int -- ^ horizontal visibility in meters, 9999 indicates a visibility over 10 km
    , directionalVariation :: [Int] -- ^ directional variation of the visibility
    , significantDirection :: Maybe Int -- ^ most operational significant direction
    } deriving (Eq, Show)

-- | Weather qualifier.
data WeatherQualifier
    = Light -- ^ light weather
    | Heavy -- ^ heavy weather
    | Vicinity -- in vicinity, i.e. not on location but within 8000 m
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
-- Precipitation: 'Drizzle' to 'UnknownPrecipitation'
-- Obscuration  : 'Mist' to 'Spray'
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
    | Mist -- ^ visibility > 1000 m
    | Fog -- ^ visibility < 1000 m
    | Smoke
    | VolcanicAsh
    | WidespreadDust
    | Sand
    | Haze
    | Spray
    | Dust -- ^ well developed dust/sand whirls
    | Squall
    | FunnelCloud -- ^ tornado, waterspout are always +fc, i.e. heavy sandstorm
    | Sandstorm
    | DustStorm
    deriving (Eq, Show)

-- | Weather observation.
data Weather = Weather
    { qualifier :: Maybe WeatherQualifier -- ^ qualifier, when absent denotes moderate weather
    , descriptor :: Maybe WeatherDescriptor -- ^ descriptor, if applicable
    , phenomenon :: [WeatherPhenomenon] -- ^phenomenon(s)
    } deriving (Eq, Show)

-- | Cloud type.
data CloudType
    = Cumulonimbus
    | ToweringCumulus
    deriving (Eq, Show)

-- | Clouds group data.
-- height of cloud base and vertical visibility are expressed in meter.
data Clouds
    = Few { height :: Int
          , cType :: Maybe CloudType }
    | Scattered { height :: Int
                , cType :: Maybe CloudType }
    | Broken { height :: Int
             , cType :: Maybe CloudType }
    | Overcast { height :: Int
               , cType :: Maybe CloudType }
    | Obscured { verticalVisibility :: Int }
    | SkyClear
    | NoCloudBelow1500
    | NoCloudBelow3600 -- ^ used in automatic observations
    deriving (Eq, Show)

-- | Mean sea level pressure unit.
data PressureUnit
    = HectoPascal
    | Inches
    deriving (Eq, Show)

-- | Mean sea level pressure.
data Pressure = Pressure
    { value :: Int
    , unit :: PressureUnit
    } deriving (Eq, Show)

-- | METAR: an aerodrome routine meteorological report.
data Metar = Metar
    { reportType :: Type -- ^ type of the report
    , corrected :: Bool -- ^ whether the report was corrected
    , auto :: Bool -- ^ whether the report contains fully automated observations without human intervention
    , missed :: Bool -- ^ whether the report corresponds to a missing report
    , station :: Aerodrome -- ^ ICAO code for the observing station (an aerodrome)
    , when :: DayTime -- ^ when the observation was made
    , wind :: Wind -- ^ wind related observations
    , cavok :: Bool -- ^ whether ceiling and visibility are OK, in which case 'visibility', 'weather' and 'clouds' are absent
    , visibility :: Maybe Visibility -- ^ visibility related observations
    , weather :: [Weather] -- ^ weather related observations
    , clouds :: [Clouds] -- ^ clouds related observations
    , temperature :: Maybe Int -- ^ temperature rounded to nearest whole degree Celsius
    , dewPoint :: Maybe Int -- ^ dew point rounded to nearest whole degree Celsius
    , pressure :: Maybe Pressure -- ^ mean sea level pressure (“QNH”)
    , remarks :: Maybe FreeText -- ^ METAR components and miscellaneous abbreviations
    } deriving (Eq, Show)
