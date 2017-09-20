-- |
-- ICAO Field Type 7 - Aircraft identification and SSR mode and code.
-- Note: according to the standard SSR mode is always A. Actual type transponder
-- is provided in Field Type 10.
module Data.Icao.F7
    ( AircraftIdentification
    , SsrCode
    , Data(aircraftIdentification, ssrCode)
    , mkAircraftIdentification
    , mkSsrCode
    , parser
    ) where

import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Aeromess.Parser
import Data.Char
import Data.Maybe ()

-- | Aircraft identification, a.k.a. call-sign, maximum of 7 uppercase/digit characters.
newtype AircraftIdentification =
    AircraftIdentification String
    deriving (Eq, Show)

-- | Secondary Surveillance Rada code, 4 octal digits.
newtype SsrCode =
    SsrCode String
    deriving (Eq, Show)

-- | Field Type 7 data.
data Data = Data
    { aircraftIdentification :: AircraftIdentification
    , ssrCode :: Maybe SsrCode
    }

ssrCodeParser' :: Parser SsrCode
ssrCodeParser' = do
    _ <- char 'A'
    fmap SsrCode (octal 4)

ssrCodeParser :: Parser (Maybe SsrCode)
ssrCodeParser = optional (slash >> ssrCodeParser')

acIdParser :: Parser AircraftIdentification
acIdParser = do
    r <- identifier
    mkAircraftIdentification r

-- | Field Type 7 parser.
parser :: Parser Data
parser = do
    acId <- acIdParser
    code <- ssrCodeParser
    _ <- dash
    return (Data acId code)

-- | 'AircraftIdentification' smart constructor. Fails if given identification is
-- not valid.
mkAircraftIdentification
    :: (MonadFail m)
    => String -> m AircraftIdentification
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
mkSsrCode
    :: (MonadFail m)
    => String -> m SsrCode
mkSsrCode c
    | validCode c = return (SsrCode c)
    | otherwise = fail ("Invalid SSR code=" ++ c ++ ", expected 4 octal digits")
