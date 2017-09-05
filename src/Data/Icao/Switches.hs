-- |
-- Functions pertaining to ICAO fields defined as a sequence of switches (i.e. F18, F19 and F22)
module Data.Icao.Switches
    ( sanitise
    )
where

import           Data.List
import           Data.Maybe

subList' :: Eq a => [a] -> [a] -> Int
subList' _ [] = -1
subList' as xxs@(x:xs)
  | all (uncurry (==)) $ zip as xxs = 0
  | otherwise                       = 1 + subList' as xs

subList :: Eq a => [[a]] -> [a] -> [Int]
subList needles haystack =
    map (`subList'` haystack) needles

indices :: Eq a => [[a]] -> [a] -> [Int]
indices needles haystack =
    filter (> 0) (map (\i -> i - 1) (sort (subList needles haystack)))

replaceBy :: Eq a => a -> [a] -> Int -> [a]
replaceBy r h i =
    let
      (x,_:ys) = splitAt i h
    in
      x ++ r : ys

sanitise''' :: Eq a => [Int] -> [a] -> a -> [a]
sanitise''' is haystack replacement =
  foldl (replaceBy replacement) haystack is

sanitise' :: Eq a => [[a]] -> [a] -> a -> [a]
sanitise' needles haystack =
  sanitise''' (indices needles haystack) haystack

switchEnd :: Char
switchEnd = '#'

splitByEnd :: String -> [String]
splitByEnd s =
    case dropWhile (== switchEnd) s of
        "" -> []
        s' -> w : splitByEnd s''
            where (w, s'') = break (== switchEnd) s'

sortByKey :: String -> String
sortByKey s =
    intercalate [switchEnd] (sort (splitByEnd s))

-- | 'sanitise' replaces the whitespace character before each '/' by a '#'
-- and sorts the string by switch key.
-- 'sanitise' facilitates the subsequent parsing of an ICAO field
-- composed of switches.
--
-- ==== __Example___
--
-- >>> sanitise ["FOO", "ARG", "BAR"] "FOO/BAR BAR BAR/HELLO ARG/HELLO-MESS/WORLD"
-- "ARG/HELLO-MESS/WORLD#BAR/HELLO#FOO/BAR BAR"
--
sanitise :: [String] -> String -> String
sanitise keys switches =
  sortByKey (sanitise' (map (++ "/") keys) switches switchEnd)
