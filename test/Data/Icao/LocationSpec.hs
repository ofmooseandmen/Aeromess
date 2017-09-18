module Data.Icao.LocationSpec
    ( spec
    ) where

import Data.Either ()
import Data.Icao.Location
import Test.Hspec

spec :: Spec
spec =
    describe "parseSignificantPoint" $ do
        it "parses LN" $ parseSignificantPoint "LN" `shouldBe` mkCodedDesignator "LN"
        it "parses MAY" $ parseSignificantPoint "MAY" `shouldBe` mkCodedDesignator "MAY"
        it "parses HADDY" $
            parseSignificantPoint "HADDY" `shouldBe` mkCodedDesignator "HADDY"
        it "parses 46N078W" $
            parseSignificantPoint "46N078W" `shouldBe` mkPosition 46.0 (-78.0)
        it "parses 46S078E" $
            parseSignificantPoint "46S078E" `shouldBe` mkPosition (-46.0) 78.0
        it "parses 4620N07805W" $
            parseSignificantPoint "4620N07805W" `shouldBe` mkPosition 46.333332 (-78.083336)
        it "parses 4620S07805E" $
            parseSignificantPoint "4620S07805E" `shouldBe` mkPosition (-46.333332) 78.083336
        it "parses DUB180040" $
            parseSignificantPoint "DUB180040" `shouldBe` mkBearingDistance "DUB" 180 40
