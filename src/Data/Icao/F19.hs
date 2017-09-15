-- |
-- ICAO Field Type 19 parser.
module Data.Icao.F19
    ( parser
    ) where

import Data.Aeromess.Parser
import Data.Char()
import Data.Either()
import Data.Icao.Lang
import Data.Icao.SupplementaryInformation
import qualified Data.Icao.Switches as S
import Data.Icao.Time
import Data.List hiding (words)
import Data.Maybe
import Prelude hiding (words)

data Data
    = Fe Hhmm
    | Pob PersonsOnBoard
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

pobParser :: Parser PersonsOnBoard
pobParser = do
    n <- natural 3 <|> natural 2 <|> natural 1
    mkPersonsOnBoard n

transmitterParser :: Parser Transmitter
transmitterParser = do
    c <- oneOf "UVE"
    return $
        case c of
            'U' -> UHF
            'V' -> VHF
            'E' -> ELT
            _   -> error "?"

atParser :: Parser [Transmitter]
atParser = some transmitterParser

survEquipParser :: Parser SurvivalEquipment
survEquipParser = do
    c <- oneOf "PDMJ"
    return $
        case c of
            'P' -> Polar
            'D' -> Desert
            'M' -> Maritime
            'J' -> Jungle
            _   -> error "?"

seParser :: Parser [SurvivalEquipment]
seParser = some survEquipParser

lfParser :: Parser LifeJacket
lfParser = do
    c <- oneOf "LF"
    return $
        case c of
            'L' -> WithLight
            'F' -> WithFluorescein
            _   -> error "?"

uvParser :: Parser LifeJacket
uvParser = do
    c <- oneOf "UV"
    return $
        case c of
            'U' -> WithRadioUHF
            'V' -> WithRadioVHF
            _   -> error "?"

ljParser :: Parser [LifeJacket]
ljParser = do
    uv <- many lfParser
    _ <- try space
    lf <- many uvParser
    return (uv ++ lf)

-- 2 NUMERICS giving the number of dinghies carried,
-- 3 NUMERICS giving the total capacity, in persons carried, of all dinghies.
-- C if dinghies are covered.
-- The colour of the dinghies (e.g. RED).
deParser :: Parser Dinghies
deParser = do
    nb <- optional (natural 2 <* space)
    capa <- optional (natural 3 <* space)
    cov <- optional (char 'C' <* space)
    col <- optional word
    mkDinghies nb capa (isJust cov) col

switchParser :: Parser (Maybe Data)
switchParser =
    S.parser E hhmmParser Fe <|>
    S.parser P pobParser Pob <|>
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
    _ <- endOfFieldParser
    return (mkSuppInformation (catMaybes s))
