module Data.Icao.OtherInformationSpec
    ( spec
    ) where

import Data.Icao.OtherInformation
import Data.Maybe ()
import Test.Hspec

spec :: Spec
spec =
    describe "mkSelCalCode" $ do
        it "accepts ABCD" $ (mkSelCalCode "ABCD" >>= show) `shouldBe` "SelCalCode \"ABCD\""
        it "accepts CDAB" $ (mkSelCalCode "CDAB" >>= show) `shouldBe` "SelCalCode \"CDAB\""
        it "rejects CDBA" $ (mkSelCalCode "CDBA" >>= show) `shouldBe` ""
        it "rejects AABC" $ (mkSelCalCode "AABC" >>= show) `shouldBe` ""
        it "rejects ABIC" $ (mkSelCalCode "ABIC" >>= show) `shouldBe` ""
        it "rejects AOBC" $ (mkSelCalCode "AOBC" >>= show) `shouldBe` ""
        it "rejects ABNC" $ (mkSelCalCode "ABNC" >>= show) `shouldBe` ""
        it "rejects AZNC" $ (mkSelCalCode "AZNC" >>= show) `shouldBe` ""
        it "rejects AZNCG" $ (mkSelCalCode "AZNCG" >>= show) `shouldBe` ""
        it "rejects A1NC" $ (mkSelCalCode "A1NC" >>= show) `shouldBe` ""
