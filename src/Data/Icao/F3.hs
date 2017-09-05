-- |
-- ICAO Field Type 3 - Message type, number and reference data.
-- TODO: support number and reference data
module Data.Icao.F3
    ( parser
    )
where

import           Text.ParserCombinators.Parsec

parser :: Parser String
parser = do
    t <- count 3 upper
    char '-'
    return t
