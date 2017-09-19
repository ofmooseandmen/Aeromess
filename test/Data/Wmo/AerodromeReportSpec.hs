module Data.Wmo.AerodromeReportSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "WMO METAR paser" $ do
        it "parses a CAVOK METAR" $
            parse "METAR LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar "LFPG" (15, 23, 30) [withWindDirection 250, withWindSpeed 3 Nothing KT]
        it "parses a METAR with basic information" $
            parse "METAR ESMS 131250Z 18016KT 9999 -RA BKN008 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [withWindDirection 180, withWindSpeed 16 Nothing KT, withPrevailingVisibility 9999]
        it "parses a METAR with wind gust" $
            parse "METAR ESMS 131250Z 18016G20KT 9999 -RA BKN008 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 (Just 20) KT
                , withPrevailingVisibility 9999
                ]
        it "parses a corrected METAR" $
            parse "METAR COR LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar
                "LFPG"
                (15, 23, 30)
                [ withModifiers (True, False, False)
                , withWindDirection 250
                , withWindSpeed 3 Nothing KT
                ]
        it "parses a corrected METAR (bis)" $
            parse "METAR LFPG 152330Z COR 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar
                "LFPG"
                (15, 23, 30)
                [ withModifiers (True, False, False)
                , withWindDirection 250
                , withWindSpeed 3 Nothing KT
                ]
        it "parses an automatic METAR" $
            parse "METAR LFPG 152330Z AUTO 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar
                "LFPG"
                (15, 23, 30)
                [ withModifiers (False, True, False)
                , withWindDirection 250
                , withWindSpeed 3 Nothing KT
                ]
        it "parses a missed METAR" $
            parse "METAR LFPG 152330Z NIL 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar
                "LFPG"
                (15, 23, 30)
                [ withModifiers (False, False, True)
                , withWindDirection 250
                , withWindSpeed 3 Nothing KT
                ]
        it "parses a METAR with lowest visibility and direction" $
            parse "METAR LFPG 152330Z 25003KT 1400 0800SW 10/09 Q1012 NOSIG" `shouldBe`
            metar
                "LFPG"
                (15, 23, 30)
                [ withWindDirection 250
                , withPrevailingVisibility 1400
                , withLowestVisibility 800 (Just SouthWest)
                , withWindSpeed 3 Nothing KT
                ]
        it "parses a METAR with prevailing visibility and Runway Visual Range (RVR)" $
            parse
                "METAR LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N R22/M0050U R27/0040 +SN BKN022 OVC050 M04/M07 Q1020 NOSIG 8849//91" `shouldBe`
            metar
                "LBBG"
                (4, 16, 0)
                [ withWindDirection 120
                , withWindSpeed 12 Nothing MPS
                , withWindVariation 90 150
                , withPrevailingVisibility 1400
                , withRunwayVisualRange "04" 1500 (Just Higher) (Just NoChange)
                , withRunwayVisualRange "22" 50 (Just Lower) (Just Up)
                , withRunwayVisualRange "27" 40 Nothing Nothing
                ]
