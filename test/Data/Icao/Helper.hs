module Data.Icao.Helper
     ( pSuccess
     , pErr
     )
where

import           Data.Icao.AtsMessage
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Error

pSuccess :: String -> AtsMessage
pSuccess text =
    either undefined id (parse parser "" text)

pErr :: String -> String
pErr text =
    either errMessage undefined (parse parser "" text)

errMessage :: ParseError -> String
errMessage e =
    "unexpected " ++ (messageString (head (errorMessages e))) ++ " at column " ++ (show (sourceColumn (errorPos e)))
