-- |
-- Aeromess Parser.
-- Allows to abstract away the choice of the underlying parser
-- library (e.g. Parsec or MegaParsec).
module Data.Aeromess.Parser
    ( Error
    , Parser
    , (<|>)
    , betweenParentheses
    , char
    , choice
    , dash
    , enumeration
    , lookAhead
    , many
    , octal
    , oneOf
    , optional
    , positive
    , parse
    , slash
    , some
    , space
    , string
    , try
    , upperNum
    , upperWord
    ) where

import Control.Monad (mplus)
import Data.Either
import Data.Functor.Identity
import Data.Maybe
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Error as E

-- | Parsing error.
data Error = Error
    { message :: String
    , column :: Int
    } deriving (Eq, Show)

-- | Parser.
type Parser a = P.ParsecT String () Identity a

-- | Tries to apply @p1@, if it fails applies @p2@.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = mplus p1 p2

-- | Parses @p@ between parentheses.
betweenParentheses :: Parser a -> Parser a
betweenParentheses = P.between (char '(') (char ')')

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
enumeration :: (Enum a, Bounded a, Show a, Read a) => Parser a
enumeration = enum' show read
  where
    enum' :: (Enum a, Bounded a) => (a -> String) -> (String -> a) -> Parser a
    enum' s r = r <$> choice (map (string . s) [minBound .. maxBound])

-- | Parses @p@ without consuming any input (unless @p@ fails).
lookAhead :: Parser a -> Parser a
lookAhead = P.lookAhead

-- | Applies the @p@ 0 or more times.
many :: Parser a -> Parser [a]
many = P.many

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
-- Returns either an 'Error' ('Left') or the parsed result ('Right').
parse :: Parser a -> String -> Either Error a
parse p s = mapLeft err (P.parse p "" s)

-- | Parses a positive number of n digits. Returns the parsed number
positive :: Int -> Parser Int
positive n = fmap read (P.count n P.digit)

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

-- | The parser @@try p@ behaves like parser @p@,
-- except that it pretends that it hasn't consumed any input when an error occurs.
try :: Parser a -> Parser a
try = P.try

-- | Parses any upper or numerical character.
upperNum :: Parser Char
upperNum = C.upper <|> C.digit

-- | Parses a word in uppercase of n character. Returns the parsed word.
upperWord :: Int -> Parser String
upperWord n = P.count n P.upper

-- Private
err :: E.ParseError -> Error
err e = Error (errMessage e) (col e)

errMessage :: E.ParseError -> String
errMessage e = "unexpected " ++ E.messageString (head (E.errorMessages e))

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft f (Right x) = Right x

col :: E.ParseError -> Int
col e = read (show (P.sourceColumn (P.errorPos e)))
