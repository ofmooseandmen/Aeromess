module Data.Icao.TimeSpec
    ( spec
    ) where

import Data.Either
import Data.Icao.Expected
import Data.Icao.Time
import Test.Hspec

spec :: Spec
spec =
    describe "parseHhmm" $ do
        it "parses 0154" $ parseHhmm "0154" `shouldBe` Right (mkHhmm' 1 54)
        it "parses 1540" $ parseHhmm "1540" `shouldBe` Right (mkHhmm' 15 40)
        it "rejects 2400" $ errorStr (parseHhmm "2400") `shouldBe` "Error {message = \"invalid hour=24\", column = 5}"
        it "rejects 154a" $ errorStr (parseHhmm "154a") `shouldBe` "Error {message = \"\\\"a\\\"\", column = 4}"
        it "rejects 154" $ errorStr (parseHhmm "154") `shouldBe` "Error {message = \"\", column = 4}"
