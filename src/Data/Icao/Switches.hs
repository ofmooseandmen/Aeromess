-- |
-- Functions pertaining to ICAO fields defined as a sequence of switches (i.e. F18, F19 and F22)
module Data.Icao.Switches
    ( sanitize
    )
where

import           Data.List
import           Data.Maybe

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

-- | 'sanitize' replaces the whitespace character before each '/' by a '-'
-- and sorts the string by switch key. Returns 'Nothing' if the given string
-- contains a '-' character or no '/' character.
-- 'sanitize' facilitates the subsequent parsing of an ICAO field
-- composed of switches.
--
-- ==== __Examples___
--
-- >>> sanitize "FOO/BAR BAR BAR/HELLO ARG/WORLD"
-- Just "ARG/WORLD-BAR/HELLO-FOO/BAR BAR"
--
-- >>> sanitize "FOO/BAR-BAR"
-- Nothing
--
-- TODO: given the context in which this method will be called, '-' and '/' are to be expected: F18-F19-...
--
sanitize :: String -> Maybe String
sanitize s
    | '-' `elem` s    = Nothing
    | '/' `notElem` s = Nothing
    | otherwise       = Just (sortByKey (setDashes s))
