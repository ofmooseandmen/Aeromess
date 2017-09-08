-- |
-- ICAO Field Type 19 - Supplementary information.
module Data.Icao.F19
    ( Transmitter(..)
    , SurvivalEquipment(..)
    , LifeJacket(..)
    , SupplementaryInformation(..)
    ) where

import Data.Aeromess.Parser
import Data.Either
import Data.Icao.Time
import Data.List
import Data.Maybe

-- | Transmitter
data Transmitter
    = UHF -- ^ frequency 243.0 MHz (UHF)
    | VHF -- ^ frequency 121.5 MHz (VHF)
    | ELT -- ^ locator transmitter (ELT)
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Survival equipment
data SurvivalEquipment
    = P -- ^  polar survival equipment
    | D -- ^  desert survival equipment
    | M -- ^  maritime survival equipment
    | J -- ^ survival equipment
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Life Jacket
data LifeJacket
    = LIGHT -- ^ life jacket equipped with lights
    | FLUORESCEIN -- ^ life jacke equipped with fluorescein
    | RADIO_UHF -- ^ life jacket radio equipped with UHF on frequency 243.0 MHz
    | RADIO_VHF -- ^ life jacket radio is equipped with VHF on frequency 121.5 MHz
    deriving (Bounded, Enum, Eq, Read, Show)

data SupplementaryInformation = SupplementaryInformation
    { fuelEndurance :: Maybe Hhmm -- ^ the fuel endurance in hours and minutes
    , personsOnBoard :: Maybe Int -- ^ the total number of persons on board, when so prescribed by the appropriate ATS authority
    , availableTransmitters :: [Transmitter] -- ^ special transmitter(s)
    , survivalEquipments :: [SurvivalEquipment] -- ^ survival equipment(s) carried on board
    , lifeJackets :: [LifeJacket] -- ^ type of life jackets carried on board
    , dinghies :: Maybe String -- ^ description of the dinghies caried on board
    , aircraftDescription :: Maybe String -- ^ the colour of the aircraft and any Significant markings (this may include the aircraft registration)
    , remarks :: Maybe String -- ^ plain language indicating any other survival equipment carried and any other useful remarks
    , pilotName :: Maybe String -- ^ the name of the pilot-in-command
    } deriving (Eq, Show)

transmitter :: Parser Transmitter
transmitter =
    enumeration :: Parser Transmitter
