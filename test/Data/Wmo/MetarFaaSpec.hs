module Data.Wmo.MetarFaaSpec
    ( spec
    ) where

import Data.Icao.Expected
import Data.Wmo.Metar
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "FAA METAR paser" $ do
        it
            "parses 'METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020'" $
            parse
                "METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
                (metar "KJFK" (15, 11, 51) $ do
                           m <- (withWindDirection 250) >> (withWindSpeed 4 Nothing KT) >> (withPrevailingVisibilityFaa Just 10 Nothing)
                           return m)
            -- Right
            --     (Metar
            --          METAR
            --          noModifiers
            --          (mkAerodrome' "KJFK")
            --          (mkDayTime' 15 11 51)
            --          (Wind (Just (WindDirection 250)) (WindSpeedKt 4) Nothing Nothing)
            --          False
            --          (Just
            --               (Visibility (VisibilityDistanceMile (Just 10) Nothing) Nothing Nothing []))
            --          []
            --          []
            --          Nothing
            --          Nothing
            --          Nothing
            --          Nothing)
        -- it
        --     "parses 'METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020'" $
        --     parse
        --         "METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
        --     Right
        --         (Metar
        --              METAR
        --              noModifiers
        --              (mkAerodrome' "KJFK")
        --              (mkDayTime' 15 11 51)
        --              (Wind (Just (WindDirection 250)) (WindSpeedKt 4) Nothing Nothing)
        --              False
        --              (Just
        --                   (Visibility
        --                        (VisibilityDistanceMile (Just 10) (Just (1, 4)))
        --                        Nothing
        --                        Nothing
        --                        []))
        --              []
        --              []
        --              Nothing
        --              Nothing
        --              Nothing
        --              Nothing)
