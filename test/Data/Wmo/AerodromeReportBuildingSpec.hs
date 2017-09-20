module Data.Wmo.AerodromeReportBuildingSpec
    ( spec
    ) where

import Data.Icao.Time
import Data.Wmo.AerodromeReport
import Test.Hspec

-- | Tests that building of METAR/SPECI messages from invalid data is rejected.
spec :: Spec
spec =
    describe "METAR/SPECI data constraints" $ do
        -- it "rejects the creation of a METAR with an invalid station" $ do
        --     msg <- case metar "!@R" (15, 23, 30) [] of
        --         Left msg -> return msg
        --         Right _ -> error "Report should not have been built"
        --     msg `shouldBe` "FOO"
        it "foo" $ do
            mkHhmm 24 00 `shouldBe` Left "FOO"
