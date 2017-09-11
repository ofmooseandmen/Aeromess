module Data.Icao.DepartureMessageSpec
    ( spec
    ) where

import Data.Either
import Data.Icao.AtsMessage
import Data.Icao.Expected
import Test.Hspec

spec :: Spec
spec =
    describe "Departure (DEP) message parser" $ do
        it "parses a DEP message containing only the mandatory fields" $
            parse "(DEP-CSA4311-EGPD1923-ENZV-0)" `shouldBe`
            Right
                (DepartureMessage
                     (mkAircraftIdentification' "CSA4311")
                     Nothing
                     Nothing
                     (mkAerodrome' "EGPD")
                     (mkHhmm' 19 23)
                     (mkAerodrome' "ENZV")
                     emptyOtherInformation)
        it "parses a DEP message containing the SSR mode and code" $
            parse "(DEP-AFR1/C3440-ESSA0800-LFPG-0)" `shouldBe`
            Right
                (DepartureMessage
                     (mkAircraftIdentification' "AFR1")
                     (Just C)
                     (Just (mkSsrCode' "3440"))
                     (mkAerodrome' "ESSA")
                     (mkHhmm' 8 0)
                     (mkAerodrome' "LFPG")
                     emptyOtherInformation)
        it "parses a DEP message containing a F18 with STS and PBN" $
            parse "(DEP-CSA4311-EGPD1923-ENZV-PBN/A1T1 STS/MARSA)" `shouldBe`
            Right
                (DepartureMessage
                     (mkAircraftIdentification' "CSA4311")
                     Nothing
                     Nothing
                     (mkAerodrome' "EGPD")
                     (mkHhmm' 19 23)
                     (mkAerodrome' "ENZV")
                     (OtherInformation
                          (Just MARSA)
                          [A1, T1]
                          Nothing
                          Nothing
                          Nothing
                          Nothing
                          Nothing
                          Nothing
                          Nothing))
        it "parses a DEP message containing a F18 with DEST and DOF" $
            parse "(DEP-CSA4311-EGPD1923-ZZZZ-DEST/ESMS DOF/870601)" `shouldBe`
            Right
                (DepartureMessage
                       (mkAircraftIdentification' "CSA4311")
                       Nothing
                       Nothing
                       (mkAerodrome' "EGPD")
                       (mkHhmm' 19 23)
                       (mkAerodrome' "ZZZZ")
                       (OtherInformation
                            Nothing
                            []
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            (Just (mkCodedDesignator' "ESMS"))
                            (Just (mkDate' 87 6 1))))
