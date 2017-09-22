module Data.Wmo.FaaAerodromeReportParsingSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Tests the parsing of FAA METAR/SPECI messages.
-- Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "FAA METAR paser" $ do
        it "parses a basic METAR" $
            parse
                "METAR KJFK 151151Z 25004KT 10SM 22/18 A3003" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withFaaPrevailingVisibility (Just 10) Nothing
                ]
        it "parses a METAR with prevailing visibility expressed in mile and fraction" $
            parse
                "METAR KJFK 151151Z 25004KT 10 1/4SM 22/18 A3003" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withFaaPrevailingVisibility (Just 10) (Just (1, 4))
                ]
        it "parses a METAR with prevailing visibility expressed in fraction only" $
            parse
                "METAR KJFK 151151Z 25004KT 1/4SM 22/18 A3003" `shouldBe`
            metar
                "KJFK"
                (15, 11, 51)
                [ withWindDirection 250
                , withWindSpeed 4 Nothing KT
                , withFaaPrevailingVisibility Nothing (Just (1, 4))
                ]
        it "parses a METAR with Runway Visual Range (RVR)" $
            parse "METAR KJFK 191921Z 06010KT 3/8SM R11L/P6000FT A3003" `shouldBe`
            metar
                "KJFK"
                (19, 19, 21)
                [ withWindDirection 60
                , withWindSpeed 10 Nothing KT
                , withFaaPrevailingVisibility Nothing (Just (3, 8))
                , withFaaRunwayVisualRange "11L" 6000 (Just Higher) Nothing
                ]
