-- |
-- ICAO Field Type 7 - Aircraft identification and SSR mode and code.
module Data.Icao.F7
    ( AircraftIdentification
    , SsrCode
    , SsrMode(..)
    , Data(aircraftIdentification, ssrMode, ssrCode)
    , mkAircraftIdentification
    , mkSsrCode
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Char
import Data.Maybe

-- | Aircraft identification, a.k.a. call-sign, maximum of 7 uppercase characters.
newtype AircraftIdentification =
    AircraftIdentification String
    deriving (Eq, Show)

-- | Secondary Surveillance Radar mode.
data SsrMode
    = A
    | C
    | S
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Secondary Surveillance Rada code, 4 octal digits.
newtype SsrCode =
    SsrCode String
    deriving (Eq, Show)

-- | Field Type 7 data.
data Data = Data
    { aircraftIdentification :: AircraftIdentification
    , ssrMode :: Maybe SsrMode
    , ssrCode :: Maybe SsrCode
    }

modeParser :: Parser SsrMode
modeParser = enumeration :: Parser SsrMode

codeParser :: Parser SsrCode
codeParser = fmap SsrCode (octal 4)

smcParser' :: Parser (SsrMode, SsrCode)
smcParser' = do
    m <- modeParser
    c <- codeParser
    return (m, c)

smcParser :: Parser (Maybe (SsrMode, SsrCode))
smcParser = optional (slash >> smcParser')

acIdParser :: Parser AircraftIdentification
acIdParser = do
    r <- some upperNum
    mkAircraftIdentification r

-- | Field Type 7 'Data' parser.
parser :: Parser Data
parser = do
    acId <- acIdParser
    smc <- smcParser
    dash
    return (Data acId (fmap fst smc) (fmap snd smc))

mkAircraftIdentification :: (Monad m) => String -> m AircraftIdentification
mkAircraftIdentification n
    | length n <= 0 = fail "empty Aicraft identification"
    | length n > 7 = fail "max Aircraft identification length (7) exceed"
    | otherwise = return (AircraftIdentification n)

validCode :: String -> Bool
validCode c
    | length c /= 4 = False
    | all isOctDigit c = True
    | otherwise = False

mkSsrCode :: (Monad m) => String -> m SsrCode
mkSsrCode c
    | validCode c = return (SsrCode c)
    | otherwise = fail "SSR code must contain 4 octal digits"
