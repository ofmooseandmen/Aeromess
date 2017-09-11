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

-- | Aircraft identification, a.k.a. call-sign, maximum of 7 uppercase/digit characters.
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
    r <- identifier
    mkAircraftIdentification r

-- | Field Type 7 parser.
parser :: Parser Data
parser = do
    acId <- acIdParser
    smc <- smcParser
    dash
    return (Data acId (fmap fst smc) (fmap snd smc))

-- | 'AircraftIdentification' smart constructor. Fails if given identification is
-- not valid.
mkAircraftIdentification :: (Monad m) => String -> m AircraftIdentification
mkAircraftIdentification s
    | null s = fail "empty aicraft identification"
    | length s > 7 = fail "max aircraft identification length (7) exceed"
    | not (all (\c -> isDigit c || isUpper c) s) = fail ("invalid aircraft identification=" ++ s)
    | otherwise = return (AircraftIdentification s)

validCode :: String -> Bool
validCode c
    | length c /= 4 = False
    | all isOctDigit c = True
    | otherwise = False

-- | 'SsrCode' smart constructor. Fails if given code value is not valid.
mkSsrCode :: (Monad m) => String -> m SsrCode
mkSsrCode c
    | validCode c = return (SsrCode c)
    | otherwise = fail ("Invalid SSR code=" ++ c ++ ", expected 4 octal digits")
