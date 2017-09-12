module Data.Icao.OtherInformationSpec
    ( spec
    ) where

import Data.Maybe()
import Data.Icao.OtherInformation
import Test.Hspec

spec :: Spec
spec =
    describe "mkSelCalCode" $ do
        it "accepts ABCD" $ (mkSelCalCode "ABCD" >>= show) `shouldBe` "SelCalCode \"ABCD\""
        it "accepts CDAB" $ (mkSelCalCode "CDAB" >>= show) `shouldBe` "SelCalCode \"CDAB\""
        it "rejects CDBA" $ (mkSelCalCode "CDBA" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "AABC" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "ABIC" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "AOBC" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "ABNC" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "AZNC" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "AZNCG" >>= show) `shouldBe` ""
        it "rejects CDBA" $ (mkSelCalCode "A1NC" >>= show) `shouldBe` ""
