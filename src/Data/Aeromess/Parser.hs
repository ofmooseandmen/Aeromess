{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Aeromess Parser.
-- Allows to abstract away the choice of the underlying parser
-- library (e.g. Parsec or MegaParsec).
module Data.Aeromess.Parser
    ( Error(message, column)
    , Parser
    , (<|>)
    , between
    , char
    , choice
    , dash
    , enumeration
    , identifier
    , lookAhead
    , many
    , manyTillSpace
    , natural
    , noneOf
    , octal
    , oneOf
    , optional
    , runParser
    , slash
    , some
    , space
    , string
    , stringTill
    , try
    , unexpected
    , word
    , words
    ) where

import Control.Monad (mplus)
import Control.Monad.Fail
import Data.Either
import Data.Functor.Identity
import Data.List hiding (words)
import Data.Maybe
import Data.Ord (comparing)
import Data.String (IsString, fromString)
import Prelude hiding (words, fail)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Error as E

-- | Parsing error.
data Error = Error
    { message :: String
    , column :: Int
    } deriving (Eq, Show)

-- | 'IsString' instance for 'Error', 'column' is set to 0.
instance IsString Error where
    fromString s = Error s 0

-- | Parser.
type Parser a = P.ParsecT String () Identity a

-- | 'MonadFail' instance for 'Parser': @fail@ calls 'unexpected'.
instance MonadFail (P.ParsecT String () Identity) where
    fail = unexpected

-- | 'MonadFail' instance for 'Either'.
instance IsString str =>
         MonadFail (Either str) where
    fail = Left . fromString

-- | Tries to apply @p1@, if it fails applies @p2@.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = mplus p1 p2

-- | Parses @p@ between given characters.
between :: Parser Char -> Parser Char -> Parser a -> Parser a
between = P.between

-- | Parses given char.
char :: Char -> Parser Char
char = C.char

-- | Tries to apply each parser until one succeeds.
choice :: [Parser a] -> Parser a
choice = P.choice

-- | Parses a '-' character
dash :: Parser Char
dash = char '-'

-- | Parses an enum representation. Returns the parsed value.
-- The parsing is done in a greedy fashion by sorting the list of
-- values by reverse order of length - i.e if possible values include
-- @A@ and @AB@, @AB@ is tried first.
enumeration
    :: (Bounded a, Enum a, Show a, Read a)
    => Parser a
enumeration = enum' show read
  where
    enum'
        :: (Bounded a, Enum a)
        => (a -> String) -> (String -> a) -> Parser a
    enum' s r =
        let sorted = sortBy (flip (comparing length)) (map s [minBound .. maxBound])
        in r <$> choice (map (try . string) sorted)

-- | Parses at 1 or more upper or numerical character(s). Returns the parsed string.
identifier :: Parser String
identifier = some (C.upper <|> C.digit)

-- | Parses @p@ without consuming any input (unless @p@ fails).
lookAhead :: Parser a -> Parser a
lookAhead = P.lookAhead

-- | Applies the @p@ 0 or more times.
many :: Parser a -> Parser [a]
many = P.many

-- | Applies the @p@ 0 or more times until a 'space'.
manyTillSpace :: Parser a -> Parser [a]
manyTillSpace p = P.manyTill p space

-- | Parses a natural number (non-negative integer) of n digits. Returns the parsed number
natural :: Int -> Parser Int
natural n = fmap read (P.count n P.digit)

-- | Dual of 'oneOf'
noneOf :: String -> Parser Char
noneOf = P.noneOf

-- | Parses @n@ octal digits.
octal :: Int -> Parser String
octal n = P.count n C.octDigit

-- | Parses any of the given characters. Return parsed character.
oneOf :: String -> Parser Char
oneOf = P.oneOf

-- | Optionally parses @p@
optional :: Parser a -> Parser (Maybe a)
optional = P.optionMaybe

-- | Parses the given text using the given parser.
-- Returns either an error message ('Left') or the parsed result ('Right').
runParser :: Parser a -> String -> Either Error a
runParser p s = mapLeft err (P.parse p "" s)

-- | Parses a '/' character
slash :: Parser Char
slash = char '/'

-- | Applies the @p@ 1 or more times.
some :: Parser a -> Parser [a]
some = P.many1

-- | Parses one whitespace character.
space :: Parser Char
space = P.space

-- | Parses the given string.
string :: String -> Parser String
string = C.string

-- | Parses any number of characters until given char
stringTill :: Char -> Parser String
stringTill c = P.manyTill C.anyChar (try (string [c]))

-- | The parser @@try p@ behaves like parser @p@,
-- except that it pretends that it hasn't consumed any input when an error occurs.
try :: Parser a -> Parser a
try = P.try

-- |  The parser @@unexpected msg@ always fails with an unexpected error message msg without consuming any input.
unexpected :: String -> Parser a
unexpected = P.unexpected

-- | Parses 1 to many uppercase character(s). Returns the parsed word.
word :: Parser String
word = some P.upper

-- | Parses 1 to many 'word' delimitted by 'space's. Returns the parsed words with spaces.
words :: Parser String
words = some (P.upper <|> space)

-- Private
err :: E.ParseError -> Error
err e = Error (errMessage e) (col e)

errMessage :: E.ParseError -> String
errMessage e = E.messageString (head (E.errorMessages e))

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

col :: E.ParseError -> Int
col e = read (show (P.sourceColumn (P.errorPos e)))
