module Data.Icao.Expected
     ( mkAerodromeName'
     , mkAircraftIdentification'
     , mkHhmm'
     )
where

import Data.Icao.AtsMessage

mkAerodromeName' :: String -> AerodromeName
mkAerodromeName' n =
    case mkAerodromeName n of
        Nothing -> error "invalid aerodrome name"
        Just r  -> r

mkAircraftIdentification' :: String -> AircraftIdentification
mkAircraftIdentification' n =
    case mkAircraftIdentification n of
        Nothing -> error "invalid aircraft identification"
        Just r  -> r

mkHhmm' :: Int -> Int -> Hhmm
mkHhmm' h m =
    case mkHhmm h m of
        Nothing -> error "invalid hour/minute"
        Just r  -> r
