-- |
-- ICAO Field Type 18 parser.
module Data.Icao.F18
    ( parser
    ) where

import Data.Aeromess.Parser
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.OtherInformation
import qualified Data.Icao.Switches as S
import Data.Icao.Time
import Data.Maybe
import Prelude hiding (words)

data Data
    = Sts SpecicalHandlingReason
    | Pbn [PbnCapabilityCode]
    | Nav FreeText
    | Com FreeText
    | Dat FreeText
    | Sur FreeText
    | Dep SignificantPoint
    | Dest SignificantPoint
    | Dof Date
    | Reg FreeText
    | Eet [EstimatedElapsedTime]
    | Sel SelCalCode

data SwitchKey
    = STS
    | PBN
    | NAV
    | COM
    | DAT
    | SUR
    | DEP
    | DEST
    | DOF
    | REG
    | EET
    | SEL
    deriving (Bounded, Enum, Eq, Read, Show)

otherInfoFiller :: Data -> OtherInformation -> OtherInformation
otherInfoFiller (Sts x) o = o {specicalHandlingReason = Just x}
otherInfoFiller (Pbn x) o = o {pbnCapabilities = x}
otherInfoFiller (Nav x) o = o {navigationEquipments = Just x}
otherInfoFiller (Com x) o = o {communicationEquipments = Just x}
otherInfoFiller (Dat x) o = o {dataCommunicationEquipments = Just x}
otherInfoFiller (Sur x) o = o {surveillanceEquipments = Just x}
otherInfoFiller (Dep x) o = o {departure = Just x}
otherInfoFiller (Dest x) o = o {destination = Just x}
otherInfoFiller (Dof x) o = o {dof = Just x}
otherInfoFiller (Reg x) o = o {registration = Just x}
otherInfoFiller (Eet x) o = o {eets = x}
otherInfoFiller (Sel x) o = o {selCalCode = Just x}


mkOtherInformation :: [Data] -> OtherInformation
mkOtherInformation = foldl (flip otherInfoFiller) emptyOtherInformation

pbnParser :: Parser [PbnCapabilityCode]
pbnParser = some (enumeration :: Parser PbnCapabilityCode)

stsParser :: Parser SpecicalHandlingReason
stsParser = enumeration :: Parser SpecicalHandlingReason

eetParser :: Parser EstimatedElapsedTime
eetParser = do
    l <- significantPointParser
    d <- hhmmParser
    return (EstimatedElapsedTime l d)

eetsParser :: Parser [EstimatedElapsedTime]
eetsParser = some eetParser

selCalParser :: Parser SelCalCode
selCalParser = word >>= mkSelCalCode

switchParser :: Parser (Maybe Data)
switchParser =
    S.parser STS stsParser Sts <|>
    S.parser PBN pbnParser Pbn <|>
    S.parser NAV freeTextParser Nav <|>
    S.parser COM freeTextParser Com <|>
    S.parser DAT freeTextParser Dat <|>
    S.parser SUR freeTextParser Sur <|>
    S.parser DEP significantPointParser Dep <|>
    S.parser DEST significantPointParser Dest <|>
    S.parser DOF dateParser Dof <|>
    S.parser REG freeTextParser Reg <|>
    S.parser EET eetsParser Eet <|>
    S.parser SEL selCalParser Sel

-- | Field Type 18 parser.
parser :: Parser OtherInformation
parser = do
    empty <- optional (char '0')
    if isJust empty
        then return emptyOtherInformation
        else do
            s <- some switchParser
            _ <- optional dash
            return (mkOtherInformation (catMaybes s))
