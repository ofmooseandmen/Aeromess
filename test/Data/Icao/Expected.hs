module Data.Icao.Expected
    ( errorStr
    , mkAerodrome'
    , mkAircraftIdentification'
    , mkBearingDistance'
    , mkCodedDesignator'
    , mkDate'
    , mkDayTime'
    , mkFreeText'
    , mkHhmm'
    , mkPosition'
    , mkSsrCode'
    ) where

import Data.Either()
import Data.Icao.AtsMessage
import Data.Maybe

errorStr :: Either Error a -> String
errorStr (Left e) = show e
errorStr _ = "?????"

mkAerodrome' :: String -> Aerodrome
mkAerodrome' n =
    fromMaybe (error "invalid aerodrome name") (mkAerodrome n)

mkAircraftIdentification' :: String -> AircraftIdentification
mkAircraftIdentification' n =
    fromMaybe
        (error "invalid aircraft identification")
        (mkAircraftIdentification n)

mkBearingDistance' :: String -> Int -> Int -> SignificantPoint
mkBearingDistance' n b d = fromMaybe (error "invalid bearing/distance") (mkBearingDistance n b d)

mkCodedDesignator' :: String -> SignificantPoint
mkCodedDesignator' n = fromMaybe (error "invalid hour/minute") (mkCodedDesignator n)

mkDate' :: Int -> Int -> Int -> Date
mkDate' y m d = fromMaybe (error "invalid year/month/day") (mkDate y m d)

mkDayTime' :: Int -> Int -> Int -> DayTime
mkDayTime' d h m = fromMaybe (error "invalid day/hour/minute") (mkDayTime d h m)

mkFreeText' :: String -> FreeText
mkFreeText' s = fromMaybe (error "invalid free text") (mkFreeText s)

mkHhmm' :: Int -> Int -> Hhmm
mkHhmm' h m = fromMaybe (error "invalid hour/minute") (mkHhmm h m)

mkPosition' :: Float -> Float -> SignificantPoint
mkPosition' lat long = fromMaybe (error "invalid latitude/longitude") (mkPosition lat long)

mkSsrCode' :: String -> SsrCode
mkSsrCode' c = fromMaybe (error "invalid SSR code") (mkSsrCode c)
