module Data.Icao.ArrivalMessageSpec (spec) where

import           Data.Icao.AtsMessage
import           Data.Icao.Expected
import           Data.Icao.Helper
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec =
    describe "Arrival (ARR) message parser" $ do
        it "parses an ARR message containing only the mandatory fields" $
            pSuccess "(ARR-CSA406-LHBP0800-LKPR0913)"
                `shouldBe` ArrivalMessage (mkAircraftIdentification' "CSA406")
                                           Nothing
                                           Nothing
                                           (mkAerodromeName' "LHBP")
                                           (mkHhmm' 8 0)
                                           Nothing
                                           (mkAerodromeName' "LKPR")
                                           (mkHhmm' 9 13)
                                           Nothing
