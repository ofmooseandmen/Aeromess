module Data.Icao.Helper
     ( parseExpectingSuccess
     , parseExpectingError
     )
where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Error

parseExpectingSuccess :: String -> Parser a -> a
parseExpectingSuccess text parser =
    either undefined id (parse parser "" text)

parseExpectingError :: String -> Parser a -> String
parseExpectingError text parser =
    either firstErrorMessage undefined (parse parser "" text)

firstErrorMessage :: ParseError -> String
firstErrorMessage err =
    messageString (head (errorMessages err))
