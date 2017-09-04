-- |
-- Operations on Air Traffic Service (ATS) messages as defined in
-- the ICAO 4444 edition 2016 standard.
--
-- TODO: better documentation...
module Data.Icao.AtsMessage
    ( AtsMessage(..)
    , parser
    )
where

import qualified Data.Icao.Time as T
import qualified Data.Icao.F7   as F7
import qualified Data.Icao.F13  as F13
import           Text.ParserCombinators.Parsec

-- | An ICAO 4444 ATS message.
data AtsMessage =
    -- | Arrival message transmitted
    -- upon reception of an arrival report by the ATS unit serving the arrival aerodrome
    -- or
    -- when a controlled flight which has experienced failure of two-way communication
    -- has landed, by the aerodrome control tower at the arrival aerodrome
    ArrivalMessage
        { -- | aircraft identification
          aircraftIndentification :: String
          -- | SSR mode
        , ssrMode                 :: Maybe F7.SsrMode
          -- | SSR code
        , ssrCode                 :: Maybe Int
          -- | aerodrome of departure
        , adep                    :: String
          -- | estimated off-block time
        , eobt                    :: T.Hhmm
          -- | aerodrome of arrival, only in case of a diversionary landing
        , originalAdes            :: Maybe String
          -- | actual aerodrome of arrival
        , adar                    :: String
          -- | actual time of arrival
        , ata                     :: T.Hhmm
          -- | name of arrival aerodrome, if 'adar' is 'ZZZZ'
        , adarName                :: Maybe String
        }
    -- | Departure message transmitted transmitted by the ATS unit serving the
    -- departure aerodrome to all recipients of basic flight plan data
    | DepartureMessage
        { -- | aircraft identification
          aircraftIndentification :: String
          -- | SSR mode
        , ssrMode                 :: Maybe F7.SsrMode
          -- | SSR code
        , ssrCode                 :: Maybe Int
          -- | aerodrome of departure
        , adep                    :: String
          -- | actual time of departure
        , atd                     :: T.Hhmm
          -- | aerodrome of arrival,
        , ades                    :: String
          -- | other information; TODO: data type instead of string
        ,  otherInformation       :: String
        }
    deriving (Show, Eq)

-- | Field Type 3 parser
-- TODO: support for message number and reference data
msgType :: Parser String
msgType = do
    t <- count 3 upper
    satisfy (== '-')
    return t

-- | 'ArrivalMessage' parser
arrival :: Parser AtsMessage
arrival = undefined

-- | 'DepartureMessage' parser
departure :: Parser AtsMessage
departure = do
    f7  <- F7.parser
    f13 <- F13.parser
    many anyChar
    return (DepartureMessage (F7.aircraftIdentification f7)
                             (F7.ssrMode f7)
                             (F7.ssrCode f7)
                             (F13.adep f13)
                             (F13.time f13)
                             ""
                             "")

-- | 'AtsMessage' parser
parser :: Parser AtsMessage
parser = do
    satisfy (== '(')
    mt <- msgType
    case mt of
        "ARR" -> arrival
        "DEP" -> departure
