{-# LANGUAGE DeriveAnyClass #-}

-- |
-- ICAO Field Type 18 - Other information.
module Data.Icao.F18
    ( OtherInformation(..)
    , PbnCapabilityCode(..)
    , SpecicalHandlingReason(..)
    , parser
    , emptyOtherInformation
    ) where

import Data.Aeromess.Parser
import Data.Maybe

-- | PBN (Performance Base Navigation) capabilities code.
-- Provides both RNAV (Area Navigation) and RNP (Required Navigation Performance) capabilities.
-- <https://en.wikipedia.org/wiki/Area_navigation>
-- <https://en.wikipedia.org/wiki/Required_navigation_performance>
data PbnCapabilityCode
    -- | RNAV
    = A1 -- ^ RNAV 10 (RNP 10)
    | B1 -- ^ RNAV 5 all permitted sensors
    | B2 -- ^ RNAV 5 GNSS
    | B4 -- ^ RNAV 5 VOR/DME
    | B3 -- ^ RNAV 5 DME/DME
    | B5 -- ^ RNAV 5 INS or IRS
    | B6 -- ^ RNAV 5 LORANC
    | C1 -- ^ RNAV 2 all permitted sensors
    | C2 -- ^ RNAV 2 GNSS
    | C3 -- ^ RNAV 2 DME/DME
    | C4 -- ^ RNAV 2 DME/DME/IRU
    | D1 -- ^ RNAV 1 all permitted sensors
    | D2 -- ^ RNAV 1 GNSS
    | D3 -- ^ RNAV 1 DME/DME
    | D4 -- ^ RNAV 1 DME/DME/IRU
    -- | RNP
    | L1 -- ^ RNP 4
    | O1 -- ^ Basic RNP 1 all permitted sensors
    | O2 -- ^ Basic RNP 1 GNSS
    | O3 -- ^ Basic RNP 1 DME/DME
    | O4 -- ^ Basic RNP 1 DME/DME/IRU
    | S1 -- ^ RNP APCH
    | S2 -- ^ RNP APCH with BAR-VNAV
    | T1 -- ^ RNP AR APCH with RF (special authorization required)
    | T2 -- ^ RNP AR APCH without RF (special authorization required
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Reason for special handling.
data SpecicalHandlingReason
    = ALTRV -- ^ for a flight operated in accordance with an altitude reservation
    | ATFMX -- ^ for a flight approved for exemption from ATFM measures by the appropriate ATS authority;
    | FFR -- ^ fire-fighting
    | FLTCK -- ^ flight check for calibration of navaids
    | HAZMAT -- ^ for a flight carrying hazardous material
    | HEAD -- ^ a flight with Head of State status
    | HOSP -- ^ for a medical flight declared by medical authorities
    | HUM -- ^ for a flight operating on a humanitarian mission
    | MARSA -- ^ for a flight for which a military entity assumes responsibility for separation of military aircraft
    | MEDEVAC -- ^ for a life critical medical emergency evacuation
    | NONRVSM -- ^ for a non-RVSM capable flight intending to operate in RVSM airspace
    | SAR -- ^ for a flight engaged in a search and rescue mission and
    | STATE -- ^ for a flight engaged in military, customs or police services
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Other information.
data OtherInformation = OtherInformation
      -- | Reason for special handling by ATS, e.g. a search and rescue mission.
    { specicalHandlingReason :: Maybe SpecicalHandlingReason
      -- | RNAV and RNP capabilites.
    , pbnCapabilities :: [PbnCapabilityCode]
      -- | Significant data related to navigation equipment, other than specified in PBN/,
      -- as required by the appropriate ATS authority. Indicate GNSS augmentation
      -- under this indicator, with a space between two or more methods of
      -- augmentation, e.g. NAV/GBAS SBAS.
    , navigationEquipments :: Maybe String
    } deriving (Show, Eq)

data Data
    = Sts SpecicalHandlingReason
    | Pbn [PbnCapabilityCode]
    | Nav String

data SwitchKey
    = PBN
    | STS
    | NAV
    deriving (Bounded, Enum, Eq, Read, Show)

emptyOtherInformation :: OtherInformation
emptyOtherInformation = OtherInformation Nothing [] Nothing

otherInfoFiller :: Data -> OtherInformation -> OtherInformation
otherInfoFiller (Sts x) o = o {specicalHandlingReason = Just x}
otherInfoFiller (Pbn x) o = o {pbnCapabilities = x}
otherInfoFiller (Nav x) o = o {navigationEquipments = Just x}

mkOtherInformation :: [Data] -> OtherInformation
mkOtherInformation = foldl (flip otherInfoFiller) emptyOtherInformation

switchKey :: SwitchKey -> Parser String
switchKey sk = string (show sk ++ "/")

switchKeys :: Parser String
switchKeys = choice (map switchKey [minBound .. maxBound])

eos :: Parser String
eos = try (optional space >> lookAhead (switchKeys <|> string "-" <|> string ")"))

sp :: SwitchKey -> Parser a -> (a -> Data) -> Parser Data
sp k p f = do
    switchKey k
    r <- p
    eos
    return (f r)

navParser :: Parser Data
navParser = sp NAV (some (upperNum <|> space)) Nav

pbnParser :: Parser Data
pbnParser = sp PBN (some (enumeration :: Parser PbnCapabilityCode)) Pbn

stsParser :: Parser Data
stsParser = sp STS (enumeration :: Parser SpecicalHandlingReason) Sts

switchParser :: Parser (Maybe Data)
switchParser = optional (pbnParser <|> stsParser <|> navParser)

parser :: Parser OtherInformation
parser = do
    pbn <- switchParser
    sts <- switchParser
    nav <- switchParser
    optional (oneOf "0-")
    return (mkOtherInformation (catMaybes [pbn, sts, nav]))
