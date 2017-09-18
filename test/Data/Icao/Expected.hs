module Data.Icao.Expected
    ( errorStr
    ) where

import Data.Either ()
import Data.Icao.AtsMessage

errorStr :: Either Error a -> String
errorStr (Left e) = show e
errorStr _ = "?????"
