module Data.Wmo.AerodromeReportWmoSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "WMO METAR paser" $ do
        it "parses 'LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG'" $
            parse "METAR LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar "LFPG" (15, 23, 30) [withWindDirection 250, withWindSpeed 3 Nothing KT]
        it "parses 'METAR ESMS 131250Z 18016KT 9999 -RA BKN008 12/12 Q0988'" $
            parse "METAR ESMS 131250Z 18016KT 9999 -RA BKN008 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibilityWmo 9999
                ]
        it
            "parses 'METAR LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N R22/M1500U +SN BKN022 OVC050 M04/M07 Q1020 NOSIG 8849//91'" $
            parse
                "METAR LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N R22/M0050U +SN BKN022 OVC050 M04/M07 Q1020 NOSIG 8849//91" `shouldBe`
            metar
                "LBBG"
                (4, 16, 0)
                [ withWindDirection 120
                , withWindSpeed 12 Nothing MPS
                , withWindVariation 90 150
                , withPrevailingVisibilityWmo 1400
                , withRunwayVisualRangeWmo "04" 1500 (Just Higher) (Just NoChange)
                , withRunwayVisualRangeWmo "22" 50 (Just Lower) (Just Up)
                ]
