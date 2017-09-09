module Data.Icao.LocationSpec
    ( spec
    ) where

import Data.Either
import Data.Icao.Expected
import Data.Icao.Location
import Test.Hspec

spec :: Spec
spec =
    describe "parseSignificantPoint" $ do
        it "parses LN" $ parseSignificantPoint "LN" `shouldBe` Right (mkCodedDesignator' "LN")
        it "parses MAY" $ parseSignificantPoint "MAY" `shouldBe` Right (mkCodedDesignator' "MAY")
        it "parses HADDY" $
            parseSignificantPoint "HADDY" `shouldBe` Right (mkCodedDesignator' "HADDY")
        it "parses 46N078W" $
            parseSignificantPoint "46N078W" `shouldBe` Right (mkPosition' 46.0 (-78.0))
        it "parses 46S078E" $
            parseSignificantPoint "46S078E" `shouldBe` Right (mkPosition' (-46.0) 78.0)
        it "parses 4620N07805W" $
            parseSignificantPoint "4620N07805W" `shouldBe` Right (mkPosition' 46.0 (-78.0))
        it "parses 4620S07805E" $
            parseSignificantPoint "4620S07805E" `shouldBe` Right (mkPosition' (-46.0) 78.0)
        it "parses DUB180040" $
            parseSignificantPoint "DUB180040" `shouldBe` Right (mkBearingDistance' "DUB" 180 40)
