-- |
-- Operations on Air Traffic Service (ATS) messages as defined in
-- the ICAO 4444 edition 2016 standard.
--
-- TODO: consider builder as in AerodromeReport.
--
-- == / Relevant links /
-- - <http://flightservicebureau.org/wp-content/uploads/2017/03/ICAO-Doc4444-Pans-Atm-16thEdition-2016-OPSGROUP.pdf ICAO 4444 Editon 2016>
-- - <http://wiki.flightgear.org/User:Johan_G/Standard_ICAO_ATS_messages_and_fields Standard ICAO ATS messages and fields>
--
module Data.Icao.AtsMessage
    ( AtsMessage(..)
    , ArrivalContent(..)
    , DepartureContent(..)
    , DelayContent(..)
    -- re-exported modules
    , module Data.Icao.Lang
    , module Data.Icao.Location
    , module Data.Icao.OtherInformation
    , module Data.Icao.SupplementaryInformation
    , module Data.Icao.Time
    -- re-exported data
    , F7.AircraftIdentification
    , F7.SsrCode
    , F8.FlightRules(..)
    , F8.FlightType(..)
    , F9.AircraftType
    , F9.WakeTurbulenceCategory(..)
    , F10.ComNavAppCapabilityCode(..)
    , F10.SurveillanceCapabilityCode(..)
    -- re-exported smart constructors
    , F7.mkAircraftIdentification
    , F7.mkSsrCode
    , F9.mkAircraftType
    -- * Parser
    , Parser
    , Error(message, column)
    , parse
    , parser
    ) where

import Data.Aeromess.Parser
import qualified Data.Icao.F10 as F10
import qualified Data.Icao.F13 as F13
import qualified Data.Icao.F16 as F16
import qualified Data.Icao.F17 as F17
import qualified Data.Icao.F18 as F18
import qualified Data.Icao.F3 as F3
import qualified Data.Icao.F7 as F7
import qualified Data.Icao.F8 as F8
import qualified Data.Icao.F9 as F9
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.OtherInformation
import Data.Icao.SupplementaryInformation
import Data.Icao.Time
import Data.Maybe ()

-- | Arrival message content.
-- An arrival message shall be transmitted:
-- upon reception of an arrival report by the ATS unit serving the arrival aerodrome,
-- or
-- when a controlled flight which has experienced failure of two-way communication
-- has landed, by the aerodrome control tower at the arrival aerodrome.
data ArrivalContent = ArrivalContent
    { arrAircraftIndentification :: F7.AircraftIdentification -- ^ aircraft identification.
    , arrSsrCode :: Maybe F7.SsrCode -- ^ SSR code.
    , arrAdep :: Aerodrome -- ^ aerodrome of departure.
    , arrEobt :: Hhmm -- ^ estimated off-block time.
    , arrAdes :: Maybe Aerodrome -- ^ aerodrome of arrival, only in case of a diversionary landing.
    , arrAdar :: Aerodrome -- ^actual aerodrome of arrival.
    , arrAta :: Hhmm -- ^ actual time of arrival.
    , arrAdarName :: Maybe FreeText -- ^ name of arrival aerodrome, if 'adar' is 'ZZZZ'.
    } deriving (Eq, Show)

-- | Departure message content.
-- A departure message shall be transmitted by the ATS unit serving the
-- departure aerodrome to all recipients of basic flight plan data.
data DepartureContent = DepartureContent
    { depAircraftIndentification :: F7.AircraftIdentification -- ^ aircraft identification.
    , depSsrCode :: Maybe F7.SsrCode -- ^ SSR code.
    , depAdep :: Aerodrome -- ^ aerodrome of departure.
    , depAtd :: Hhmm -- ^ actual time of departure.
    , depAdes :: Aerodrome -- ^ aerodrome of arrival.
    , depOtherInformation :: OtherInformation -- ^ other information.
    } deriving (Eq, Show)

-- | Delay message content.
-- A delay message shall be transmitted when the departure of an aircraft, for which basic flight plan data has been sent,
-- is delayed by more than 30 minutes after the estimated off-block time contained in the basic flight plan data.
data DelayContent = DelayContent
    { dlaAircraftIndentification :: F7.AircraftIdentification -- ^ aircraft identification.
    , dlaSsrCode :: Maybe F7.SsrCode -- ^ SSR code.
    , dlaAdep :: Aerodrome -- ^ aerodrome of departure.
    , dlaEobt :: Hhmm -- ^ revised estimated off-block time.
    , dlaAdes :: Aerodrome -- ^ aerodrome of arrival.
    , dlaOtherInformation :: OtherInformation -- ^ other information.
    } deriving (Eq, Show)

-- | ICAO 4444 ATS message.
data AtsMessage
    = Arr ArrivalContent -- ^ Arrival message.
    | Dep DepartureContent -- ^ Departure message.
    | Dla DelayContent -- ^ Delay message.
    deriving (Eq, Show)

-- | 'ArrivalMessage' parser.
arrParser :: Parser AtsMessage
arrParser = do
    f7 <- F7.parser
    f13 <- F13.parser
    ades <- optional (try F16.adesParser)
    f17 <- F17.parser
    return
        (Arr
             (ArrivalContent
                  (F7.aircraftIdentification f7)
                  (F7.ssrCode f7)
                  (F13.adep f13)
                  (F13.time f13)
                  ades
                  (F17.adar f17)
                  (F17.ata f17)
                  (F17.adarName f17)))

-- | common parser for DEP and DLA messages.
depParser' :: Parser (F7.AircraftIdentification, Maybe F7.SsrCode, Aerodrome, Hhmm, Aerodrome, OtherInformation)
depParser' = do
    f7 <- F7.parser
    f13 <- F13.parser
    f16Ades <- F16.adesParser
    f18 <- F18.parser
    return (F7.aircraftIdentification f7, F7.ssrCode f7, F13.adep f13, F13.time f13, f16Ades, f18)

-- | 'DepartureMessage' parser.
depParser :: Parser AtsMessage
depParser = do
    c <- depParser'
    let (ai, sc, d, t, s, o) = c
    return (Dep (DepartureContent ai sc d t s o))

-- | 'DelayMessage' parser.
dlaParser :: Parser AtsMessage
dlaParser = do
    c <- depParser'
    let (ai, sc, d, t, s, o) = c
    return (Dla (DelayContent ai sc d t s o))

-- | ATS message content parser - i.e. everything between '(' and ')'
contentParser :: Parser AtsMessage
contentParser = do
    f3 <- F3.parser
    case f3 of
        "ARR" -> arrParser
        "DEP" -> depParser
        "DLA" -> dlaParser
        _ -> unexpected ("unexpected message=" ++ show f3)

-- | 'AtsMessage' parser.
parser :: Parser AtsMessage
parser = between (char '(') (char ')') contentParser

-- | Parses the given textual representation of an 'AtsMessage'.
-- return either an 'Error' ('Left') or the parsed 'AtsMessage' ('Right').
parse :: String -> Either Error AtsMessage
parse = runParser parser
