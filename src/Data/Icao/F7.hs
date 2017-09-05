-- |
-- ICAO Field Type 7 - Aircraft identification and SSR mode and code.
module Data.Icao.F7
    ( AircraftIdentification ()
    , SsrMode (..)
    , Data (aircraftIdentification, ssrMode, ssrCode)
    , mkAircraftIdentification
    , parser
    )
where

import           Data.Maybe
import           Text.ParserCombinators.Parsec

-- | Aircraft identification, a.k.a. call-sign, maximum of 7 uppercase characters.
data AircraftIdentification = AircraftIdentification String deriving (Show, Eq)

-- | Secondary Surveillance Radar Mode.
data SsrMode =
      A
    | C
    | S
    deriving (Show, Eq, Enum)

-- | Field Type 7 data.
data Data = Data
    { aircraftIdentification :: AircraftIdentification
    , ssrMode                :: Maybe SsrMode
    , ssrCode                :: Maybe Int
    }

modeParser :: Parser SsrMode
modeParser = do
    m <- oneOf "ACS"
    return $ case m of
        'A' -> A
        'C' -> C
        'S' -> S

codeParser :: Parser Int
codeParser =
    fmap read (count 4 digit)

smcParser' :: Parser (SsrMode, Int)
smcParser' = do
    m <- modeParser
    c <- codeParser
    return (m, c)

smcParser :: Parser (Maybe (SsrMode, Int))
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
    satisfy (== '-')
    return (Data acId (fmap fst smc) (fmap snd smc))

mkAircraftIdentification :: (Monad m) => String -> m AircraftIdentification
mkAircraftIdentification n
    | length n <= 0 = fail "empty Aicraft identification"
    | length n > 7  = fail "max Aircraft identification length (7) exceed"
    | otherwise     = return (AircraftIdentification n)
