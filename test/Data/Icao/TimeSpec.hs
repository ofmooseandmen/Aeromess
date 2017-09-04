module Data.Icao.TimeSpec (spec) where

import           Data.Icao.Time
import           Data.Icao.Helper
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec =
    describe "Time" $
        describe "hhmmParser" $ do
            it "parses 0154" $
                pSucces "0154" `shouldBe` Hhmm 1 54
            it "rejects 0abc" $
                pErr "0abc" `shouldBe` "\"a\""
            it "rejects 2400" $
                pErr "2400" `shouldBe` "Invalid hour"
            it "rejects an empty string" $
                pErr "" `shouldBe` ""

pSucces :: String -> Hhmm
pSucces text =
    parseExpectingSuccess text hhmmParser

pErr :: String -> String
pErr text =
    parseExpectingError text hhmmParser
