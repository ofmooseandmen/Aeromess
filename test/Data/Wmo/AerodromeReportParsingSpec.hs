module Data.Wmo.AerodromeReportParsingSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Tests the parsing of WMO METAR/SPECI messages.
-- Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "WMO METAR paser" $ do
        it "parses a CAVOK METAR" $
            parse "METAR LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar "LFPG" (15, 23, 30) [withWindDirection 250, withWindSpeed 3 Nothing KT]
        it "parses a CAVOK METAR (bis)" $
            fmap cavok (parse "METAR LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG") `shouldBe`
            Right True
        it "parses a METAR with 'calm' conditions" $
            parse "METAR LFPG 152330Z 00000KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            metar "LFPG" (15, 23, 30) []
        it "parses a METAR with basic information" $
            parse "METAR ESMS 131250Z 18016KT 9999 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [withWindDirection 180, withWindSpeed 16 Nothing KT, withPrevailingVisibility 9999]
        it "parses a METAR with wind gust and cloud amount" $
            parse "METAR ESMS 131250Z 18016G20KMH 9999 BKN008 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 (Just 20) KMH
                , withPrevailingVisibility 9999
                , withCloudAmount Broken (Just 8) Nothing
                ]
        it "parses a METAR with variable wind" $
            parse "METAR ESMS 131250Z VRB16KT 9999 12/12 Q0988" `shouldBe`
            metar "ESMS" (13, 12, 50) [withWindSpeed 16 Nothing KT, withPrevailingVisibility 9999]
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
                "METAR LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N R11/1200D R22/M0050U R27/0040 M04/M07 Q1020 NOSIG 8849//91" `shouldBe`
            metar
                "LBBG"
                (4, 16, 0)
                [ withWindDirection 120
                , withWindSpeed 12 Nothing MPS
                , withWindVariation 90 150
                , withPrevailingVisibility 1400
                , withRunwayVisualRange "04" 1500 (Just Higher) (Just NoChange)
                , withRunwayVisualRange "11" 1200 Nothing (Just Down)
                , withRunwayVisualRange "22" 50 (Just Lower) (Just Up)
                , withRunwayVisualRange "27" 40 Nothing Nothing
                ]
        it "parses a METAR with light rain" $
            parse "METAR ESMS 131250Z 18016KT 9999 -RA 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withWeather (Just LightWeather) Nothing [Rain]
                ]
        it "parses a METAR with patches of fog, heavy mist and snow in vicinity" $
            parse "METAR ESMS 131250Z 18016KT 9999 BCFG +BR VCSN 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withWeather Nothing (Just Patches) [Fog]
                , withWeather (Just HeavyWeather) Nothing [Mist]
                , withWeather (Just InVicinityWeather) Nothing [Snow]
                ]
        it "parses a SPECI with heavy rain and few/overcast cloud amounts" $
            parse "SPECI ESMS 131250Z 18016KT 9999 +RA FEW022CB OVC/// 12/12 Q0988" `shouldBe`
            speci
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withWeather (Just HeavyWeather) Nothing [Rain]
                , withCloudAmount Few (Just 22) (Just Cumulonimbus)
                , withCloudAmount Overcast Nothing Nothing
                ]
        it "parses a METAR with obscured sky and vertical visibility" $
            parse "METAR ESMS 131250Z 18016KT 9999 VV015 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withObscuredSky (Just 15)
                ]
        it "parses a SPECI with obscured sky and unknown vertical visibility" $
            parse "SPECI ESMS 131250Z 18016KT 9999 VV/// 12/12 Q0988" `shouldBe`
            speci
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withObscuredSky Nothing
                ]
        it "parses a METAR with clear sky (NCD)" $
            parse "METAR ESMS 131250Z 18016KT 9999 NCD 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withNoCloudObserved SkyClear
                ]
        it "parses a METAR with clear sky (SKC)" $
            parse "METAR ESMS 131250Z 18016KT 9999 SKC 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withNoCloudObserved SkyClear
                ]
        it "parses a METAR with no observed cloud below 1500 ft" $
            parse "METAR ESMS 131250Z 18016KT 9999 NSC 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withNoCloudObserved NoCloudBelow1500
                ]
        it "parses a METAR with no observed cloud below 3600 ft" $
            parse "METAR ESMS 131250Z 18016KT 9999 CLR 12/12 Q0988" `shouldBe`
            metar
                "ESMS"
                (13, 12, 50)
                [ withWindDirection 180
                , withWindSpeed 16 Nothing KT
                , withPrevailingVisibility 9999
                , withNoCloudObserved NoCloudBelow3600
                ]
