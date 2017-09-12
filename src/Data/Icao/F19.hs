-- |
-- ICAO Field Type 19 - Supplementary information.
-- This field is a collection of switches all optional, use the withXXXX function to
-- build a new instance starting from 'emptySupplementaryInformation'.
module Data.Icao.F19
    ( Dinghies(..)
    , Transmitter(..)
    , SurvivalEquipment(..)
    , LifeJacket(..)
    , SupplementaryInformation(..)
    , parser
    -- | 'SupplementaryInformation' builder functions
    , emptySupplementaryInformation
    , withFuelEndurance
    , withPersonsOnBoard
    , witAvailableTransmitters
    , withSurvivalEquipments
    , withLifeJackets
    , withDinghies
    , withAircraftDescription
    , withOtherRemarks
    , withPilotInCommand
    ) where

import Data.Aeromess.Parser
import Data.Char
import Data.Either
import Data.Icao.Lang
import qualified Data.Icao.Switches as S
import Data.Icao.Time
import Data.List hiding (words)
import Data.Maybe
import Prelude hiding (words)

-- | Transmitter
data Transmitter
    = UHF -- ^ frequency 243.0 MHz (UHF)
    | VHF -- ^ frequency 121.5 MHz (VHF)
    | ELT -- ^ locator transmitter (ELT)
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Survival equipment
data SurvivalEquipment
    = POLAR -- ^  polar survival equipment
    | DESERT -- ^  desert survival equipment
    | MARITIME -- ^  maritime survival equipment
    | JUNGLE -- ^ jungle survival equipment
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Life Jacket
data LifeJacket
    = LIGHT -- ^ life jacket equipped with lights
    | FLUORESCEIN -- ^ life jacke equipped with fluorescein
    | RADIO_UHF -- ^ life jacket radio equipped with UHF on frequency 243.0 MHz
    | RADIO_VHF -- ^ life jacket radio is equipped with VHF on frequency 121.5 MHz
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Dinghies
data Dinghies = Dinghies
    { number :: Maybe Natural2 -- ^ the number of dinghies carried.
    , totalCapacity :: Maybe Natural3 -- ^  the total capacity, in persons carried, of all dinghies.
    , covered :: Bool -- ^ whether dinghies are covered.
    , colour :: Maybe FreeText -- ^ the colour of the dinghies.
    } deriving (Eq, Show)

-- | Supplementary information data.
data SupplementaryInformation = SupplementaryInformation
    { fuelEndurance :: Maybe Hhmm -- ^ the fuel endurance in hours and minutes
    , personsOnBoard :: Maybe Natural3 -- ^ the total number of persons on board, when so prescribed by the appropriate ATS authority
    , availableTransmitters :: [Transmitter] -- ^ special transmitter(s)
    , survivalEquipments :: [SurvivalEquipment] -- ^ survival equipment(s) carried on board
    , lifeJackets :: [LifeJacket] -- ^ type of life jackets carried on board
    , dinghies :: Dinghies -- ^ description of the dinghies caried on board
    , aircraftDescription :: Maybe FreeText -- ^ the colour of the aircraft and any Significant markings (this may include the aircraft registration)
    , otherRemarks :: Maybe FreeText -- ^ plain language indicating any other survival equipment carried and any other useful remarks
    , pilotInCommand :: Maybe FreeText -- ^ the name of the pilot-in-command and possibly the contact phone
    } deriving (Eq, Show)

data Data
    = Fe Hhmm
    | Pob Natural3
    | At [Transmitter]
    | Se [SurvivalEquipment]
    | Lj [LifeJacket]
    | Di Dinghies
    | Ad FreeText
    | Rmk FreeText
    | Pn FreeText

data SwitchKey
    = E
    | P
    | R
    | S
    | J
    | D
    | A
    | N
    | C
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Returns empty 'SupplementaryInformation'.
emptySupplementaryInformation :: SupplementaryInformation
emptySupplementaryInformation =
    SupplementaryInformation Nothing Nothing [] [] [] (Dinghies Nothing Nothing False Nothing) Nothing Nothing Nothing

-- | Sets the aircraft fuel endurance in hours and minutes.
withFuelEndurance :: Hhmm -> SupplementaryInformation -> SupplementaryInformation
withFuelEndurance fe si = si {fuelEndurance = Just fe}

-- | Sets the number of persons on board the aircraft.
withPersonsOnBoard :: Natural3 -> SupplementaryInformation -> SupplementaryInformation
withPersonsOnBoard n si = si {personsOnBoard = Just n}

-- | Sets the available transmitters on board the aircraft.
witAvailableTransmitters :: [Transmitter] -> SupplementaryInformation -> SupplementaryInformation
witAvailableTransmitters t si = si {availableTransmitters = t}

-- | Sets the available survival equipements on board the aircraft.
withSurvivalEquipments ::
       [SurvivalEquipment] -> SupplementaryInformation -> SupplementaryInformation
withSurvivalEquipments s si = si {survivalEquipments = s}

-- | Sets the aircraft description.
withAircraftDescription :: FreeText -> SupplementaryInformation -> SupplementaryInformation
withAircraftDescription d si = si {aircraftDescription = Just d}

-- | Sets the detail of the pilot in command.
withPilotInCommand :: FreeText -> SupplementaryInformation -> SupplementaryInformation
withPilotInCommand p si = si {pilotInCommand = Just p}

-- | Sets the dinghies.
withDinghies :: Dinghies -> SupplementaryInformation -> SupplementaryInformation
withDinghies d si = si {dinghies = d}

-- | Sets the details of the life jackets available on board the aircraft.
withLifeJackets :: [LifeJacket] -> SupplementaryInformation -> SupplementaryInformation
withLifeJackets l si = si {lifeJackets = l}

-- | Sets the othe remarks.
withOtherRemarks :: FreeText -> SupplementaryInformation -> SupplementaryInformation
withOtherRemarks r si = si {pilotInCommand = Just r}

suppInfoFiller :: Data -> SupplementaryInformation -> SupplementaryInformation
suppInfoFiller (Fe x) o = o {fuelEndurance = Just x}
suppInfoFiller (Pob x) o = o {personsOnBoard = Just x}
suppInfoFiller (At x) o = o {availableTransmitters = x}
suppInfoFiller (Se x) o = o {survivalEquipments = x}
suppInfoFiller (Lj x) o = o {lifeJackets = x}
suppInfoFiller (Di x) o = o {dinghies = x}
suppInfoFiller (Ad x) o = o {aircraftDescription = Just x}
suppInfoFiller (Rmk x) o = o {otherRemarks = Just x}
suppInfoFiller (Pn x) o = o {pilotInCommand = Just x}

mkSuppInformation :: [Data] -> SupplementaryInformation
mkSuppInformation = foldl (flip suppInfoFiller) emptySupplementaryInformation

transmitterParser :: Parser Transmitter
transmitterParser = do
    c <- oneOf "UVE"
    return $
        case c of
            'U' -> UHF
            'V' -> VHF
            'E' -> ELT

atParser :: Parser [Transmitter]
atParser = some transmitterParser

survEquipParser :: Parser SurvivalEquipment
survEquipParser = do
    c <- oneOf "PDMJ"
    return $
        case c of
            'P' -> POLAR
            'D' -> DESERT
            'M' -> MARITIME
            'J' -> JUNGLE

seParser :: Parser [SurvivalEquipment]
seParser = some survEquipParser

lfParser :: Parser LifeJacket
lfParser = do
    c <- oneOf "LF"
    return $
        case c of
            'L' -> LIGHT
            'F' -> FLUORESCEIN

uvParser :: Parser LifeJacket
uvParser = do
    c <- oneOf "UV"
    return $
        case c of
            'U' -> RADIO_UHF
            'V' -> RADIO_VHF

ljParser :: Parser [LifeJacket]
ljParser = do
    uv <- many lfParser
    try space
    lf <- many uvParser
    return (uv ++ lf)

-- 2 NUMERICS giving the number of dinghies carried,
-- 3 NUMERICS giving the total capacity, in persons carried, of all dinghies.
-- C if dinghies are covered.
-- The colour of the dinghies (e.g. RED).
deParser :: Parser Dinghies
deParser = do
    nb <- optional (natural2Parser <* space)
    capa <- optional (natural3Parser <* space)
    cov <- optional (char 'C' <* space)
    col <- optional freeTextParser
    return (Dinghies nb capa (isJust cov) col)

switchParser :: Parser (Maybe Data)
switchParser =
    S.parser E hhmmParser Fe <|>
    S.parser P natural3Parser Pob <|>
    S.parser R atParser At <|>
    S.parser S seParser Se <|>
    S.parser J ljParser Lj <|>
    S.parser D deParser Di <|>
    S.parser A freeTextParser Ad <|>
    S.parser N freeTextParser Rmk <|>
    S.parser C freeTextParser Pn

-- | 'SupplementaryInformation' parser.
parser :: Parser SupplementaryInformation
parser = do
    s <- some switchParser
    optional dash
    return (mkSuppInformation (catMaybes s))
