module Data.Icao.DepartureMessageSpec
    ( spec
    ) where

import Data.Either
import Data.Icao.AtsMessage
import Data.Icao.Expected
import Test.Hspec

spec :: Spec
spec = do
    describe "Departure (DEP) message parser" $ do
        it "parses a DEP message containing only the mandatory fields" $
            parse parser "(DEP-CSA4311-EGPD1923-ENZV-0)" `shouldBe`
            Right
                (DepartureMessage
                     (mkAircraftIdentification' "CSA4311")
                     Nothing
                     Nothing
                     (mkAerodromeName' "EGPD")
                     (mkHhmm' 19 23)
                     (mkAerodromeName' "ENZV")
                     emptyOtherInformation)
        it "parses a DEP message containing the SSR mode and code" $
            parse parser "(DEP-AFR1/C3440-ESSA0800-LFPG-0)" `shouldBe`
            Right
                (DepartureMessage
                     (mkAircraftIdentification' "AFR1")
                     (Just C)
                     (Just (mkSsrCode' "3440"))
                     (mkAerodromeName' "ESSA")
                     (mkHhmm' 8 0)
                     (mkAerodromeName' "LFPG")
                     emptyOtherInformation)
        it "parses a DEP message containing a F18 with STS and PBN" $
            parse parser "(DEP-CSA4311-EGPD1923-ENZV-STS/MARSA PBN/A1T1)" `shouldBe`
            Right
                (DepartureMessage
                     (mkAircraftIdentification' "CSA4311")
                     Nothing
                     Nothing
                     (mkAerodromeName' "EGPD")
                     (mkHhmm' 19 23)
                     (mkAerodromeName' "ENZV")
                     (OtherInformation (Just MARSA) [A1, T1] Nothing))
