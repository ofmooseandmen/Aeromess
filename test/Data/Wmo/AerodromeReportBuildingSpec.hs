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
            Left "invalid visibility distance [metre]=10000"
        it "rejects the creation of a METAR with a negative prevailing visibility" $
            metar "ESMS" (29, 0, 0) [withPrevailingVisibility (-1)] `shouldBe`
            Left "invalid visibility distance [metre]=-1"
        it "rejects the creation of a METAR with an invalid runway designator (less than 2 characters)" $
            metar "ESMS" (29, 0, 0) [withRunwayVisualRange "1" 40 Nothing Nothing] `shouldBe`
            Left "invalid runway designator=1"
        it "rejects the creation of a METAR with an invalid runway designator (???)" $
            metar "ESMS" (29, 0, 0) [withRunwayVisualRange "???" 40 Nothing Nothing] `shouldBe`
            Left "invalid runway designator=???"
        it "rejects the creation of a METAR with an invalid runway designator (13X)" $
            metar "ESMS" (29, 0, 0) [withRunwayVisualRange "13X" 40 Nothing Nothing] `shouldBe`
            Left "invalid runway designator=13X"
        it "rejects the creation of a METAR with a negative vertical visibility" $
            metar "ESMS" (29, 0, 0) [withObscuredSky (Just (-1))] `shouldBe`
            Left "invalid vertical visibility [hundreds feet]=-1"
        it "rejects the creation of a METAR with an invalid vertical visibility" $
            metar "ESMS" (29, 0, 0) [withObscuredSky (Just 1000)] `shouldBe`
            Left "invalid vertical visibility [hundreds feet]=1000"
        it "rejects the creation of a METAR with a negative cloud amount height" $
            metar "ESMS" (29, 0, 0) [withCloudAmount Few (Just (-1)) Nothing] `shouldBe`
            Left "invalid cloud amount height [hundreds feet]=-1"
        it "rejects the creation of a METAR with an invalid cloud amount height" $
            metar "ESMS" (29, 0, 0) [withCloudAmount Few (Just 1000) Nothing] `shouldBe`
            Left "invalid cloud amount height [hundreds feet]=1000"
        it "rejects the creation of a METAR with an invalid temperature" $
            metar "ESMS" (29, 0, 0) [withTemperature 100 10] `shouldBe`
            Left "invalid temperature [celsius]=100"
        it "rejects the creation of a METAR with an dew point" $
            metar "ESMS" (29, 0, 0) [withTemperature 10 100] `shouldBe`
            Left "invalid temperature [celsius]=100"
        it "rejects the creation of a METAR with a negative pressure" $
            metar "ESMS" (29, 0, 0) [withPressure (-1)] `shouldBe`
            Left "invalid pressure [Hpa]=-1"
        it "rejects the creation of a METAR with an invalid pressure" $
            metar "ESMS" (29, 0, 0) [withPressure 10000] `shouldBe`
            Left "invalid pressure [Hpa]=10000"
