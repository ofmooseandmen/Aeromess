-- |
-- Core data types and parsers.
module Data.Icao.Lang
    ( FreeText
    , endOfFieldParser
    , freeTextParser
    , mkFreeText
    ) where

import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Aeromess.Parser

-- | Free text, containing any character but '/', '-' and parentheses.
newtype FreeText =
    FreeText String
    deriving (Eq, Show)

-- | 'FreeText' parser
freeTextParser :: Parser FreeText
freeTextParser = fmap FreeText (some (noneOf "-/()"))

-- | A field is terminated '-' followed by some free text that needs to
-- be further parsed. If the next character is not a '-' this signifies the end
-- of the message and the main parser will expected a ')'.
-- This is specially usefull when the parsed field may or may not be a terminal field.
-- TODO: consider using this for all fields.
endOfFieldParser :: Parser ()
endOfFieldParser = do
    d <- optional dash
    case d of
        Nothing -> return ()
        Just _
                -- a '-' was found make sure there's something coming next
         -> do
            _ <- lookAhead freeTextParser
            return ()

-- | 'FreeText' smart constructor. Fails if given string is not a valid free text.
mkFreeText
    :: (MonadFail m)
    => String -> m FreeText
mkFreeText s
    | null s = fail "empty free text"
    | any (\c -> c == '/' || c == '-' || c == '(' || c == ')') s = fail ("invalid free text=" ++ s)
    | otherwise = return (FreeText s)
