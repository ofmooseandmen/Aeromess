-- |
-- Utility module
-- TODO: consider breaking down
module Data.Icao.Util
    ( betterSwitches
    , aerodromeParser
    )

where

import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.Parsec

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

spaceBefore :: String -> Int -> Maybe Int
spaceBefore s i =
    let
        sub = take (i - 1) s
    in if null sub then
        Nothing
    else
       safeLast (elemIndices ' ' sub)

replaceSpaceByDash :: String -> Int -> String
replaceSpaceByDash s i =
    let
      (x,_:ys) = splitAt i s
    in
      x ++ '-' : ys

replaceSpaceBeforeByDash :: String -> Int -> String
replaceSpaceBeforeByDash s i =
    maybe s (replaceSpaceByDash s) (spaceBefore s i)

setDashes' :: [Int] -> String -> String
setDashes' is s =
    foldl replaceSpaceBeforeByDash s is

setDashes :: String -> String
setDashes s =
    setDashes' (elemIndices '/' s) s

splitByDash :: String -> [String]
splitByDash s =
    case dropWhile (== '-') s of
        "" -> []
        s' -> w : splitByDash s''
            where (w, s'') = break (== '-') s'

sortByKey :: String -> String
sortByKey s =
    intercalate "-" (sort (splitByDash s))

-- | 'betterSwitches' replaces the whitespace character before each '/' by a '-'
-- and sorts the string by switch key. Returns 'Nothing' if the given string
-- contains a '-' character or no '/' character.
-- 'betterSwitches' is facilitates the subsequent parsing of an ICAO field
-- composed of switches.
--
-- ==== __Examples___
--
-- >>> preProcess "FOO/BAR BAR BAR/HELLO ARG/WORLD"
-- Just "ARG/WORLD-BAR/HELLO-FOO/BAR BAR"
--
-- >>> preProcess "FOO/BAR-BAR"
-- Nothing
--
betterSwitches :: String -> Maybe String
betterSwitches s
    | '-' `elem` s    = Nothing
    | '/' `notElem` s = Nothing
    | otherwise       = Just (sortByKey (setDashes s))

-- | Parser for ICAO compliant aerordome names (4 uppecrase characters)
aerodromeParser :: Parser String
aerodromeParser =
    count 4 upper
