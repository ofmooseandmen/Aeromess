module Data.Icao.Expected
    ( mkAerodromeName'
    , mkAircraftIdentification'
    , mkHhmm'
    , mkSsrCode'
    ) where

import Data.Icao.AtsMessage
import Data.Maybe

mkAerodromeName' :: String -> AerodromeName
mkAerodromeName' n =
    fromMaybe (error "invalid aerodrome name") (mkAerodromeName n)

mkAircraftIdentification' :: String -> AircraftIdentification
mkAircraftIdentification' n =
    fromMaybe
        (error "invalid aircraft identification")
        (mkAircraftIdentification n)

mkSsrCode' :: String -> SsrCode
mkSsrCode' c = fromMaybe (error "invalid SSR code") (mkSsrCode c)

mkHhmm' :: Int -> Int -> Hhmm
mkHhmm' h m = fromMaybe (error "invalid hour/minute") (mkHhmm h m)
