-- |
-- ICAO Field Type 19 - Supplementary information.
module Data.Icao.F19
    ( Dinghies(..)
    , Transmitter(..)
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
    { number :: Maybe Int -- ^ the number of dinghies carried.
    , totalCapacity :: Maybe Int -- ^  the total capacity, in persons carried, of all dinghies.
    , covered :: Bool -- ^ whether dinghies are covered.
    , colour :: Maybe String -- ^ the colour of the dinghies.
    } deriving (Eq, Show)

data SupplementaryInformation = SupplementaryInformation
    { fuelEndurance :: Maybe Hhmm -- ^ the fuel endurance in hours and minutes
    , personsOnBoard :: Maybe Int -- ^ the total number of persons on board, when so prescribed by the appropriate ATS authority
    , availableTransmitters :: [Transmitter] -- ^ special transmitter(s)
    , survivalEquipments :: [SurvivalEquipment] -- ^ survival equipment(s) carried on board
    , lifeJackets :: [LifeJacket] -- ^ type of life jackets carried on board
    , dinghies :: Dinghies -- ^ description of the dinghies caried on board
    , aircraftDescription :: Maybe String -- ^ the colour of the aircraft and any Significant markings (this may include the aircraft registration)
    , remarks :: Maybe String -- ^ plain language indicating any other survival equipment carried and any other useful remarks
    , pilotName :: Maybe String -- ^ the name of the pilot-in-command
    } deriving (Eq, Show)

data Data
    = Fe Hhmm
    | Pob Int
    | At [Transmitter]
    | Se [SurvivalEquipment]
    | Lj [LifeJacket]
    | Di Dinghies
    | Ad String
    | Rmk String
    | Pn String

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

emptyDinghies :: Dinghies
emptyDinghies = Dinghies Nothing Nothing False Nothing

emptySupplementaryInformation :: SupplementaryInformation
emptySupplementaryInformation =
    SupplementaryInformation Nothing Nothing [] [] [] emptyDinghies Nothing Nothing Nothing

suppInfoFiller :: Data -> SupplementaryInformation -> SupplementaryInformation
suppInfoFiller (Fe x) o = o {fuelEndurance = Just x}
suppInfoFiller (Pob x) o = o {personsOnBoard = Just x}
suppInfoFiller (At x) o = o {availableTransmitters = x}
suppInfoFiller (Se x) o = o {survivalEquipments = x}
suppInfoFiller (Lj x) o = o {lifeJackets = x}
suppInfoFiller (Di x) o = o {dinghies = x}
suppInfoFiller (Ad x) o = o {aircraftDescription = Just x}
suppInfoFiller (Rmk x) o = o {remarks = Just x}
suppInfoFiller (Pn x) o = o {pilotName = Just x}

mkSuppInformation :: [Data] -> SupplementaryInformation
mkSuppInformation = foldl (flip suppInfoFiller) emptySupplementaryInformation

-- TODO the 4 following methods are duplicated with F18
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

feParser :: Parser Data
feParser = sp E hhmmParser Fe

pobParser :: Parser Data
pobParser = sp P (positive 1 <|> positive 2 <|> positive 3) Pob

atParser :: Parser Data
atParser = sp R undefined At

seParser :: Parser Data
seParser = sp S undefined Se

ljParser :: Parser Data
ljParser = sp J undefined Lj

diParser :: Parser Data
diParser = sp D undefined Di

adParser :: Parser Data
adParser = sp A (some (upperNum <|> space)) Ad

rmkParser :: Parser Data
rmkParser = sp N (some (upperNum <|> space)) Rmk

pnParser :: Parser Data
pnParser = sp C undefined Pn

switchParser :: Parser (Maybe Data)
switchParser =
    optional
        (feParser  <|>
         pobParser <|>
         atParser  <|>
         seParser  <|>
         ljParser  <|>
         diParser  <|>
         adParser  <|>
         rmkParser <|>
         pnParser)

transmitter :: Parser Transmitter
transmitter = enumeration :: Parser Transmitter

parser :: Parser SupplementaryInformation
parser = do
    fe <- switchParser
    pob <- switchParser
    at <- switchParser
    se <- switchParser
    lj <- switchParser
    di <- switchParser
    ad <- switchParser
    rmk <- switchParser
    pn <- switchParser
    optional dash
    return (mkSuppInformation (catMaybes [fe, pob, at, se, lj, di, ad, rmk, pn]))
