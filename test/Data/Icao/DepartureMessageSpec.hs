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
        it "parses a DEP message containing the SSR mode and code" $
            pSuccess "(DEP-AFR1/C3440-ESSA0800-LFPG-0)"
                `shouldBe` DepartureMessage (mkAircraftIdentification' "AFR1")
                                            (Just C)
                                            (Just (mkSsrCode' "3440"))
                                            (mkAerodromeName' "ESSA")
                                            (mkHhmm' 8 0)
                                            (mkAerodromeName' "LFPG")
                                            Nothing
