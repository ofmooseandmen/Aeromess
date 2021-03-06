-- |
-- ICAO Field Type 9 - Number and type of aircraft and wake turbulence category.
module Data.Icao.F9
    ( AircraftType
    , WakeTurbulenceCategory(..)
    , Data(acNbr, acType, wtc)
    , mkAircraftType
    , acParser
    , parser
    ) where

import Control.Monad.Fail
import Data.Aeromess.Parser
import Data.Char
import Data.Maybe (fromMaybe)
import Prelude hiding (fail)

-- | Aircraft type, e.g. A320
newtype AircraftType =
    AircraftType String
    deriving (Eq, Show)

-- | Wake Turbulence Category.
data WakeTurbulenceCategory
    = Light -- ^ light
    | Medium -- ^ medium
    | Heavy -- ^ heavy
    | Jumbo -- ^ Jumbo
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Field 9  data.
data Data = Data
    { acNbr :: Int -- this need to be AcNb
    , acType :: AircraftType
    , wtc :: WakeTurbulenceCategory
    }

-- | 'AircraftType' smart constructor. Fails if given identification is
-- not valid.
mkAircraftType
    :: (MonadFail m)
    => String -> m AircraftType
mkAircraftType s
    | length s < 2 || length s > 4 = fail "aircraft type must be 2-4 characters"
    | not (all (\c -> isDigit c || isUpper c) s) = fail ("invalid aircraft type=" ++ s)
    | otherwise = return (AircraftType s)

-- | aircraft number and type parser.
acParser :: Parser (Int, AircraftType)
acParser = do
    n <- optional (try (natural 1 <|> natural 2))
    a <- identifier >>= mkAircraftType
    return (fromMaybe 1 n, a)

wtcParser :: Parser WakeTurbulenceCategory
wtcParser = do
    c <- oneOf "LMHJ"
    return $
        case c of
            'L' -> Light
            'M' -> Medium
            'H' -> Heavy
            'J' -> Jumbo
            _ -> error "?"

-- | Field Type 9 parser.
parser :: Parser Data
parser = do
    na <- acParser
    w <- wtcParser
    return (uncurry Data na w)
