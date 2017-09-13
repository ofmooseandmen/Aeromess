-- |
-- ICAO Field Type 8 - Flight rules and type of flight.
module Data.Icao.F8
    ( FlightRules(..)
    , FlightType(..)
    , Data(frules, ftype)
    , parser
    ) where

import Data.Aeromess.Parser

-- | Flight rules.
-- Note: If the letter Y or Z is used, the point or points at which a change of
-- flight rules is planned is to be shown as indicated in Field Type 15.
data FlightRules
    = IFR -- ^ if it is intended that the entire flight will be operated under the IFR
    | VFR -- ^ if it is intended that the entire flight will be operated under the VFR
    | YFR -- ^ if the flight initially will be operated under the IFR followed by one or
          -- more subsequent changes of flight rules
    | ZFR -- ^ if the flight initially will be operated under the VFR followed by one or
          -- more subsequent changes of flight rules
    deriving (Bounded, Enum, Eq, Read, Show)

-- more subsequent changes of flight rules
-- more subsequent changes of flight rules
-- | Type of flight
data FlightType
    = Scheduled -- ^ if scheduled air transport
    | NonScheduled -- ^ if non-scheduled air transport
    | General -- ^ if general aviation
    | Military -- ^ if military
    | Other -- ^ other flights
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Filed Type 8 data.
data Data = Data
    { frules :: FlightRules
    , ftype :: Maybe FlightType
    }

flightRulesParser :: Parser FlightRules
flightRulesParser = do
    c <- oneOf "IVYZ"
    return $
        case c of
            'I' -> IFR
            'V' -> VFR
            'Y' -> YFR
            'Z' -> ZFR
            _ -> error "?"

flightTypeParser :: Parser FlightType
flightTypeParser = do
    c <- oneOf "SNGMX"
    return $
        case c of
            'S' -> Scheduled
            'N' -> NonScheduled
            'G' -> General
            'M' -> Military
            'X' -> Other
            _ -> error "?"

-- | Field Type 8 parser.
parser :: Parser Data
parser = do
    fr <- flightRulesParser
    ft <- optional flightTypeParser
    _ <- dash
    return (Data fr ft)
