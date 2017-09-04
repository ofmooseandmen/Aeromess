-- |
-- ICAO Field Type 18 - Other information.
module Data.Icao.F18
    ( parser
    )
where

import           Text.ParserCombinators.Parsec

f18 :: Parser String
f18 =
    many1 (noneOf "-)")

parser :: Parser (Maybe String)
parser = do
    r <- optionMaybe (noneOf "0" >> f18)
    oneOf "0-)"
    return r
