-- |
-- ICAO Field Type 3 - Message type, number and reference data.
-- TODO: support number and reference data
module Data.Icao.F3
    ( parser
    ) where

import Data.Aeromess.Parser

parser :: Parser String
parser = do
    t <- upperWord 3
    dash
    return t