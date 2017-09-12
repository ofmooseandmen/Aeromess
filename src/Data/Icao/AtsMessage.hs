-- |
-- Operations on Air Traffic Service (ATS) messages as defined in
-- the ICAO 4444 edition 2016 standard.
--
-- TODO: better documentation here and also in each FXX indicate if it can be a terminal field
module Data.Icao.AtsMessage
    ( AtsMessage(..)
    -- re-exported data types
    , Aerodrome
    , Date(year, month, day)
    , FreeText
    , Natural2
    , Natural3
    , Hhmm(hour, minute)
    , SignificantPoint(CodedDesignator, Position, BearingDistance,
                 latitude, longitude, reference, bearing, distance)
    , F7.AircraftIdentification
    , F7.SsrCode
    , F7.SsrMode(..)
    , F18.OtherInformation(..)
    , F18.SpecicalHandlingReason(..)
    , F18.PbnCapabilityCode(..)
    -- re-exported smart constructors
    , mkAerodrome
    , mkDate
    , mkFreeText
    , mkNatural2
    , mkNatural3
    , mkHhmm
    , mkCodedDesignator
    , mkPosition
    , mkBearingDistance
    , F7.mkAircraftIdentification
    , F7.mkSsrCode
    , F18.emptyOtherInformation
    -- re-exported Parser
    , Parser
    , Error(message, column)
    , parse
    , parser
    ) where

import Data.Aeromess.Parser
import qualified Data.Icao.F13 as F13
import qualified Data.Icao.F16 as F16
import qualified Data.Icao.F17 as F17
import qualified Data.Icao.F18 as F18
import qualified Data.Icao.F3 as F3
import qualified Data.Icao.F7 as F7
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time
import Data.Maybe

-- | An ICAO 4444 ATS message.
data AtsMessage
    -- | Arrival message transmitted:
    -- upon reception of an arrival report by the ATS unit serving the arrival aerodrome,
    -- or
    -- when a controlled flight which has experienced failure of two-way communication
    -- has landed, by the aerodrome control tower at the arrival aerodrome.
    = ArrivalMessage { aircraftIndentification :: F7.AircraftIdentification -- ^ aircraft identification.
                     , ssrMode :: Maybe F7.SsrMode -- ^ SSR mode.
                     , ssrCode :: Maybe F7.SsrCode -- ^ SSR code.
                     , adep :: Aerodrome -- ^ aerodrome of departure.
                     , eobt :: Hhmm -- ^ estimated off-block time.
                     , originalAdes :: Maybe Aerodrome -- ^ aerodrome of arrival, only in case of a diversionary landing.
                     , adar :: Aerodrome -- ^actual aerodrome of arrival
                     , ata :: Hhmm -- ^ actual time of arrival.
                     , adarName :: Maybe String -- ^ name of arrival aerodrome, if 'adar' is 'ZZZZ'.
                      }
    -- | Departure message transmitted transmitted by the ATS unit serving the
    -- departure aerodrome to all recipients of basic flight plan data.
    | DepartureMessage { aircraftIndentification :: F7.AircraftIdentification -- ^ aircraft identification.
                       , ssrMode :: Maybe F7.SsrMode -- ^ SSR mode.
                       , ssrCode :: Maybe F7.SsrCode -- ^ SSR code.
                       , adep :: Aerodrome -- ^ aerodrome of departure.
                       , atd :: Hhmm -- ^ actual time of departure.
                       , ades :: Aerodrome -- ^ aerodrome of arrival.
                       , otherInformation :: F18.OtherInformation -- ^ other information.
                        }
    deriving (Eq, Show)

-- | PARSERS.
-- | 'ArrivalMessage' parser.
arrParser :: Parser AtsMessage
arrParser = do
    f7 <- F7.parser
    f13 <- F13.parser
    orginalAdes <- optional (try F16.adesParser)
    f17 <- F17.parser
    return
        (ArrivalMessage
             (F7.aircraftIdentification f7)
             (F7.ssrMode f7)
             (F7.ssrCode f7)
             (F13.adep f13)
             (F13.time f13)
             orginalAdes
             (F17.adar f17)
             (F17.ata f17)
             (F17.adarName f17))

-- | 'DepartureMessage' parser.
depParser :: Parser AtsMessage
depParser = do
    f7 <- F7.parser
    f13 <- F13.parser
    ades <- F16.adesParser
    f18 <- F18.parser
    return
        (DepartureMessage
             (F7.aircraftIdentification f7)
             (F7.ssrMode f7)
             (F7.ssrCode f7)
             (F13.adep f13)
             (F13.time f13)
             ades
             f18)

-- | ATS message content parser - i.e. everything between '(' and ')'
contentParser :: Parser AtsMessage
contentParser = do
    f3 <- F3.parser
    case f3 of
        "ARR" -> arrParser
        "DEP" -> depParser

-- | 'AtsMessage' parser.
parser :: Parser AtsMessage
parser = betweenParentheses contentParser

-- | Parses the given textual representation of an 'AtsMessage'.
-- return either an 'Error' ('Left') or the parsed 'AtsMessage' ('Right').
parse :: String -> Either Error AtsMessage
parse s = runParser parser s
