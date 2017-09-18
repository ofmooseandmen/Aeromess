module Data.Wmo.AerodromeReportFaaSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "FAA METAR paser" $ do
        it
            "parses 'METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020'" $
            parse
                "METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withPrevailingVisibilityFaa (Just 10) Nothing
                ]
        it
            "parses 'METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020'" $
            parse
                "METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withPrevailingVisibilityFaa (Just 10) (Just (1, 4))
                ]
