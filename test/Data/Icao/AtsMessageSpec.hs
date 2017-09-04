module Data.Icao.AtsMessageSpec (spec) where

import           Data.Icao.AtsMessage
import           Data.Icao.Helper
import           Data.Icao.Time
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec =
    describe "parser" $ do
        describe "DEP message" $
            it "parses (DEP-CSA4311-EGPD1923-ENZV-0)" $
                pSuccess "(DEP-CSA4311-EGPD1923-ENZV-0)"
                    `shouldBe` DepartureMessage "CSA4311"
                                                 Nothing
                                                 Nothing
                                                 "EGPD"
                                                 (Hhmm 19 23)
                                                 "ENZV"
                                                 Nothing

        describe "ARR message" $
            it "parses (ARR-CSA406-LHBP0800-LKPR0913)" $
                pSuccess "(ARR-CSA406-LHBP0800-LKPR0913)"
                    `shouldBe` ArrivalMessage "CSA406"
                                              Nothing
                                              Nothing
                                              "LHBP"
                                              (Hhmm 8 0)
                                              Nothing
                                              "LKPR"
                                              (Hhmm 9 13)
                                              Nothing

pSuccess :: String -> AtsMessage
pSuccess text =
    parseExpectingSuccess text parser
