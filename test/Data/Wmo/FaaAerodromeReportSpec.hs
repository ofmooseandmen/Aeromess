module Data.Wmo.FaaAerodromeReportSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "FAA METAR paser" $ do
        it
            "parses a basic METAR" $
            parse
                "METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withFaaPrevailingVisibility (Just 10) Nothing
                ]
        it
            "parses a METAR with prevailing visibility expressed in mile and fraction" $
            parse
                "METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withFaaPrevailingVisibility (Just 10) (Just (1, 4))
                ]
        it
            "parses a METAR with prevailing visibility expressed in fraction only" $
            parse
                "METAR KJFK 151151Z 25004KT 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withFaaPrevailingVisibility Nothing (Just (1, 4))
                ]
