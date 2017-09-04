module Data.Icao.AtsMessageSpec (spec) where

import           Data.Icao.AtsMessage
import           Data.Icao.Helper
import           Data.Icao.Time
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec =
    describe "parser" $
        describe "DEP message" $
            it "parses (DEP-CSA4311-EGPD1923-ENZV-0)" $
                pSuccess "(DEP-CSA4311-EGPD1923-ENZV-0)"
                    `shouldBe` DepartureMessage "CSA4311"
                                                 Nothing
                                                 Nothing
                                                 "EGPD"
                                                 (Hhmm 19 23)
                                                 "ENZV"
                                                 "0"

pSuccess :: String -> AtsMessage
pSuccess text =
    parseExpectingSuccess text parser
