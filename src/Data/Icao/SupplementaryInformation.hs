-- |
-- Provides data types and functions pertaining to the supplementary information
-- (Field Type 19) as defined in the ICAO 4444 edition 2016 standard.
module Data.Icao.SupplementaryInformation
    ( Transmitter(..)
    , SurvivalEquipment(..)
    , LifeJacket(..)
    , SupplementaryInformation(..)
    )
where

import           Data.Either
import           Data.Icao.Time
import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.Parsec

-- | Transmitter
data Transmitter =
      UHF -- ^ frequency 243.0 MHz (UHF)
    | VHF -- ^ frequency 121.5 MHz (VHF)
    | ELT -- ^ locator transmitter (ELT)
    deriving (Show, Eq, Enum)

-- | Survival equipment
data SurvivalEquipment =
      P -- ^  polar survival equipment
    | D -- ^  desert survival equipment
    | M -- ^  maritime survival equipment
    | J -- ^ survival equipment
    deriving (Show, Eq, Enum)

-- | Life Jacket
data LifeJacket =
      LIGHT -- ^ life jacket equipped with lights
    | FLUORESCEIN -- ^ life jacke equipped with fluorescein
    | RADIO_UHF -- ^ life jacket radio equipped with UHF on frequency 243.0 MHz
    | RADIO_VHF -- ^ life jacket radio is equipped with VHF on frequency 121.5 MHz
    deriving (Show, Eq, Enum)

data SupplementaryInformation = SupplementaryInformation
     { fuelEndurance         :: Maybe Hhmm -- ^ the fuel endurance in hours and minutes
     , personsOnBoard        :: Maybe Int -- ^ the total number of persons on board, when so prescribed by the appropriate ATS authority
     , availableTransmitters :: [Transmitter] -- ^ special transmitter(s)
     , survivalEquipments    :: [SurvivalEquipment] -- ^ survival equipment(s) carried on board
     , lifeJackets           :: [LifeJacket] -- ^ type of life jackets carried on board
     , dinghies              :: Maybe String -- ^ description of the dinghies caried on board
     , aircraftDescription   :: Maybe String -- ^ the colour of the aircraft and any Significant markings (this may include the aircraft registration)
     , remarks               :: Maybe String -- ^ plain language indicating any other survival equipment carried and any other useful remarks
     , pilotName             :: Maybe String -- ^ the name of the pilot-in-command
     } deriving (Show, Eq)

transmitter :: Parser Transmitter
transmitter = do
         parse <- oneOf "UVE"
         return $ case parse of
             'U' -> UHF
             'V' -> VHF
             'E' -> ELT
