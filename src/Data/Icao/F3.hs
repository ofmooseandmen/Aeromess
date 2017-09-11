-- |
-- ICAO Field Type 3 - Message type, number and reference data.
-- TODO: support number and reference data
module Data.Icao.F3
    ( parser
    ) where

import Data.Aeromess.Parser

-- | Field Type 3 parser.
parser :: Parser String
parser = do
    t <- word
    dash
    return t
