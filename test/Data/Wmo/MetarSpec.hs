module Data.Wmo.MetarSpec
    ( spec
    ) where

import Data.Wmo.Metar
import Test.Hspec

spec :: Spec
spec = do
    describe "mkRunwayDesignator" $ do
        it "accepts 01" $ (mkRunwayDesignator "01" >>= show) `shouldBe` "RunwayDesignator \"01\""
        it "accepts 14L" $ (mkRunwayDesignator "14L" >>= show) `shouldBe` "RunwayDesignator \"14L\""
        it "accepts 03C" $ (mkRunwayDesignator "03C" >>= show) `shouldBe` "RunwayDesignator \"03C\""
        it "accepts 23R" $ (mkRunwayDesignator "23R" >>= show) `shouldBe` "RunwayDesignator \"23R\""
        it "rejects 03LQ" $ (mkRunwayDesignator "03LQ" >>= show) `shouldBe` ""
        it "rejects 1AC" $ (mkRunwayDesignator "1AC" >>= show) `shouldBe` ""
        it "rejects 01A" $ (mkRunwayDesignator "01A" >>= show) `shouldBe` ""
