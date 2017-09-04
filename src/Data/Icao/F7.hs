-- |
-- ICAO Field Type 7 - Aircraft identification and SSR mode and code.
module Data.Icao.F7
    ( SsrMode (..)
    , Data (..)
    , parser
    )
where

import           Data.Maybe
import           Text.ParserCombinators.Parsec

data SsrMode =
      A
    | C
    | S
    deriving (Show, Eq, Enum)

data Data = Data
    { aircraftIdentification :: String
    , ssrMode                :: Maybe SsrMode
    , ssrCode                :: Maybe Int
    }

mode :: Parser SsrMode
mode = do
    m <- oneOf "ACS"
    return $ case m of
        'A' -> A
        'C' -> C
        'S' -> S

code :: Parser Int
code =
    fmap read (count 4 digit)

ssrModeAndCode' :: Parser (SsrMode, Int)
ssrModeAndCode' = do
    m <- mode
    c <- code
    return (m, c)

ssrModeAndCode :: Parser (Maybe (SsrMode, Int))
ssrModeAndCode =
    optionMaybe (char '/' >> ssrModeAndCode')

parser :: Parser Data
parser = do
    acId <- count 7 alphaNum
    smc <- ssrModeAndCode
    satisfy (== '-')
    return (Data acId (fmap fst smc) (fmap snd smc))
