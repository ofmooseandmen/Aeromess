module Data.Wmo.AerodromeReportBuildingSpec
    ( spec
    ) where

import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Tests that building of METAR/SPECI messages from invalid data is rejected.
spec :: Spec
spec =
    describe "METAR/SPECI data constraints" $ do
        it "rejects the creation of a METAR with an invalid station" $
            metar "!@R" (15, 23, 30) [] `shouldBe`
            Left "invalid aerodrome name=!@R expected 4 [A-Z] characters"
        it "rejects the creation of a METAR with an invalid day" $
            metar "ESMS" (32, 23, 30) [] `shouldBe` Left "invalid day=32"
        it "rejects the creation of a METAR with an invalid hour" $
            metar "ESMS" (17, 24, 30) [] `shouldBe` Left "invalid hour=24"
        it "rejects the creation of a METAR with an invalid minute" $
            metar "ESMS" (17, 23, 60) [] `shouldBe` Left "invalid minute=60"
        it "rejects the creation of a METAR with an invalid wind direction" $
            metar "ESMS" (17, 21, 24) [withWindDirection 361] `shouldBe`
            Left "invalid wind direction [degrees]=361"
        it "rejects the creation of a METAR with a negative wind direction" $
            metar "ESMS" (17, 21, 24) [withWindDirection (-1)] `shouldBe`
            Left "invalid wind direction [degrees]=-1"
        it "rejects the creation of a METAR with an invalid wind speed" $
            metar "ESMS" (17, 21, 24) [withWindSpeed 100 Nothing KT] `shouldBe`
            Left "invalid wind speed [KT]=100"
        it "rejects the creation of a METAR with an invalid prevailing visibility" $
            metar "ESMS" (29, 0, 0) [withPrevailingVisibility 10000] `shouldBe`
            Left "invalid visibility distance [meter]=10000"
        it "rejects the creation of a METAR with a negative prevailing visibility" $
            metar "ESMS" (29, 0, 0) [withPrevailingVisibility (-1)] `shouldBe`
            Left "invalid visibility distance [meter]=-1"
        it "rejects the creation of a METAR with an invalid runway designator" $
            metar "ESMS" (29, 0, 0) [withRunwayVisualRange "???" 40 Nothing Nothing] `shouldBe`
            Left "invalid runway designator=???"
