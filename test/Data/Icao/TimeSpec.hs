module Data.Icao.TimeSpec
    ( spec
    ) where

import Data.Either ()
import Data.Icao.Expected
import Data.Icao.Time
import Test.Hspec

spec :: Spec
spec = do
    describe "parseHhmm" $ do
        it "parses 0154" $ parseHhmm "0154" `shouldBe` mkHhmm 1 54
        it "parses 1540" $ parseHhmm "1540" `shouldBe` mkHhmm 15 40
        it "rejects 2400" $
            errorStr (parseHhmm "2400") `shouldBe`
            "Error {message = \"invalid hour=24\", column = 5}"
        it "rejects 154a" $
            errorStr (parseHhmm "154a") `shouldBe` "Error {message = \"\\\"a\\\"\", column = 4}"
        it "rejects 154" $
            errorStr (parseHhmm "154") `shouldBe` "Error {message = \"\", column = 4}"
    describe "parseDate" $ do
        it "parses 790901" $ parseDate "790901" `shouldBe` mkDate 79 9 1
        it "rejects 790b01" $
            errorStr (parseDate "790b01") `shouldBe` "Error {message = \"\\\"b\\\"\", column = 4}"
        it "rejects 791301" $
            errorStr (parseDate "791301") `shouldBe`
            "Error {message = \"invalid month=13\", column = 7}"
        it "rejects 790932" $
            errorStr (parseDate "790932") `shouldBe`
            "Error {message = \"invalid day=32\", column = 7}"
