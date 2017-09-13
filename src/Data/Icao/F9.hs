-- |
-- ICAO Field Type 9 - Number and type of aircraft and wake turbulence category.
module Data.Icao.F9
    ( AircraftType
<<<<<<< HEAD
    , WakeTurbulenceCategory
=======
    , WakeTurbulenceCategory(..)
>>>>>>> F9 + DLA
    , Data(acNbr, acType, wtc)
    , mkAircraftType
    , acParser
    , parser
    ) where

import Data.Aeromess.Parser
import Data.Char
import Data.Icao.Lang

-- | Aircraft type, e.g. A320
newtype AircraftType =
    AircraftType String
    deriving (Eq, Show)

-- | Wake Turbulence Category.
data WakeTurbulenceCategory
<<<<<<< HEAD
    = L -- ^ light
    | M -- ^ medium
    | H -- ^ heavy
    | J -- ^ Jumbo
=======
    = LIGHT -- ^ light
    | MEDIUM -- ^ medium
    | HEAVY -- ^ heavy
    | JUMBO -- ^ Jumbo
>>>>>>> F9 + DLA
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Field 9  data.
data Data = Data
    { acNbr :: Natural2
    , acType :: AircraftType
    , wtc :: WakeTurbulenceCategory
    }

-- | 'AircraftType' smart constructor. Fails if given identification is
-- not valid.
mkAircraftType :: (Monad m) => String -> m AircraftType
mkAircraftType s
    | length s < 2 || length s > 4 = fail "aircraft type must be 2-4 characters"
    | not (all (\c -> isDigit c || isUpper c) s) = fail ("invalid aircraft type=" ++ s)
    | otherwise = return (AircraftType s)

oneAsDefault :: (Monad m) => Maybe Natural2 -> m Natural2
oneAsDefault n =
    case n of
        Nothing -> mkNatural2 1
        Just x -> return x

-- | aircraft number and type parser.
acParser :: Parser (Natural2, AircraftType)
acParser = do
    n <- optional (try natural2Parser) >>= oneAsDefault
    a <- identifier >>= mkAircraftType
    return (n, a)

<<<<<<< HEAD
=======
wtcParser :: Parser WakeTurbulenceCategory
wtcParser = do
    c <- oneOf "LMHJ"
    return $
        case c of
            'L' -> LIGHT
            'M' -> MEDIUM
            'H' -> HEAVY
            'J' -> JUMBO
            _ -> error "?"

>>>>>>> F9 + DLA
-- | Field Type 9 parser.
parser :: Parser Data
parser = do
    na <- acParser
<<<<<<< HEAD
    w <- enumeration :: Parser WakeTurbulenceCategory
=======
    w <- wtcParser
>>>>>>> F9 + DLA
    return (uncurry Data na w)
