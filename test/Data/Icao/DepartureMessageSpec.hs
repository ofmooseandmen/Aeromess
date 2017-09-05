module Data.Icao.DepartureMessageSpec (spec) where

import           Data.Icao.AtsMessage
import           Data.Icao.Expected
import           Data.Icao.Helper
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec =
    describe "Departure (DEP) message parser" $ do
        it "parses a DEP message containing only the mandatory fields" $
            pSuccess "(DEP-CSA4311-EGPD1923-ENZV-0)"
                `shouldBe` DepartureMessage (mkAircraftIdentification' "CSA4311")
                                             Nothing
                                             Nothing
                                             (mkAerodromeName' "EGPD")
                                             (mkHhmm' 19 23)
                                             (mkAerodromeName' "ENZV")
                                             Nothing
