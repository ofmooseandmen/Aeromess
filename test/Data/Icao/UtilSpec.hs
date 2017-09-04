module Data.Icao.UtilSpec (spec) where

import           Data.Icao.Util
import           Test.Hspec

spec :: Spec
spec =
    describe "Util" $
        describe "betterSwitches" $ do
            it "turns 'S3/FOO  S2/ FOO  BAR S1/BAR BAR' into 'S1/BAR BAR-S2/ FOO  BAR-S3/FOO '" $
                betterSwitches "S3/FOO  S2/ FOO  BAR S1/BAR BAR"
                    `shouldBe` Just "S1/BAR BAR-S2/ FOO  BAR-S3/FOO "
            it "ignores an empty string" $
                betterSwitches "" `shouldBe` Nothing
            it "ignores a string with no switch: FOO BAR" $
                betterSwitches "FOO BAR" `shouldBe` Nothing
            it "ignores a string with '-'" $
                betterSwitches "F/FOO-BAR B/B F/OOF" `shouldBe` Nothing
