-- |
-- ICAO Field Type 7 - Aircraft identification and SSR mode and code.
module Data.Icao.F7
    ( AircraftIdentification
    , SsrCode
    , SsrMode (..)
    , Data (aircraftIdentification, ssrMode, ssrCode)
    , mkAircraftIdentification
    , mkSsrCode
    , parser
    )
where

import           Data.Char
import           Data.Maybe
import           Text.ParserCombinators.Parsec

-- | Aircraft identification, a.k.a. call-sign, maximum of 7 uppercase characters.
newtype AircraftIdentification = AircraftIdentification String deriving (Show, Eq)

-- | Secondary Surveillance Radar mode.
data SsrMode =
      A
    | C
    | S
    deriving (Show, Eq, Enum)

-- | Secondary Surveillance Rada code, 4 octal digits.
newtype SsrCode = SsrCode String deriving (Show, Eq)

-- | Field Type 7 data.
data Data = Data
    { aircraftIdentification :: AircraftIdentification
    , ssrMode                :: Maybe SsrMode
    , ssrCode                :: Maybe SsrCode
    }

modeParser :: Parser SsrMode
modeParser = do
    m <- oneOf "ACS"
    return $ case m of
        'A' -> A
        'C' -> C
        'S' -> S

codeParser :: Parser SsrCode
codeParser =
    fmap SsrCode (count 4 octDigit)

smcParser' :: Parser (SsrMode, SsrCode)
smcParser' = do
    m <- modeParser
    c <- codeParser
    return (m, c)

smcParser :: Parser (Maybe (SsrMode, SsrCode))
smcParser =
    optionMaybe (char '/' >> smcParser')

acIdParser :: Parser AircraftIdentification
acIdParser = do
    r <- many alphaNum
    if length r > 7 then
        unexpected "Invalid aircraft identification"
    else
        return (AircraftIdentification r)

-- | Field Type 7 'Data' parser.
parser :: Parser Data
parser = do
    acId <- acIdParser
    smc <- smcParser
    char '-'
    return (Data acId (fmap fst smc) (fmap snd smc))

mkAircraftIdentification :: (Monad m) => String -> m AircraftIdentification
mkAircraftIdentification n
    | length n <= 0 = fail "empty Aicraft identification"
    | length n > 7  = fail "max Aircraft identification length (7) exceed"
    | otherwise     = return (AircraftIdentification n)

validCode :: String -> Bool
validCode c
    | length c /= 4    = False
    | all isOctDigit c = True
    | otherwise        = False

mkSsrCode :: (Monad m) => String -> m SsrCode
mkSsrCode c
    | validCode c = return (SsrCode c)
    | otherwise   = fail "SSR code must contain 4 octal digits"
