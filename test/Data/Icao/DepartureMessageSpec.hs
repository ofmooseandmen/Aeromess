module Data.Icao.DepartureMessageSpec
    ( spec
    ) where

import Data.Either ()
import Data.Function ((&))
import Data.Icao.AtsMessage
import Data.Maybe (fromJust)
import Test.Hspec

spec :: Spec
spec =
    describe "Departure (DEP) message parser" $ do
        it "parses a DEP message containing only the mandatory fields" $
            parse "(DEP-CSA4311-EGPD1923-ENZV-0)" `shouldBe`
            Right
                (Dep
                     (DepartureContent
                          (fromJust (mkAircraftIdentification "CSA4311"))
                          Nothing
                          (fromJust (mkAerodrome "EGPD"))
                          (fromJust (mkHhmm 19 23))
                          (fromJust (mkAerodrome "ENZV"))
                          emptyOtherInformation))
        it "parses a DEP message containing the SSR mode and code" $
            parse "(DEP-AFR1/A3440-ESSA0800-LFPG-0)" `shouldBe`
            Right
                (Dep
                     (DepartureContent
                          (fromJust (mkAircraftIdentification "AFR1"))
                          (Just (fromJust (mkSsrCode "3440")))
                          (fromJust (mkAerodrome "ESSA"))
                          (fromJust (mkHhmm 8 0))
                          (fromJust (mkAerodrome "LFPG"))
                          emptyOtherInformation))
        it "parses a DEP message containing a F18 with STS and PBN" $
            parse "(DEP-CSA4311-EGPD1923-ENZV-PBN/A1T1 STS/MARSA)" `shouldBe`
            Right
                (Dep
                     (DepartureContent
                          (fromJust (mkAircraftIdentification "CSA4311"))
                          Nothing
                          (fromJust (mkAerodrome "EGPD"))
                          (fromJust (mkHhmm 19 23))
                          (fromJust (mkAerodrome "ENZV"))
                          (emptyOtherInformation & withSpecialHandlingReason MARSA .
                           withPbnCapabilities [A1, T1])))
        it "parses a DEP message containing a F18 with DEST and DOF" $
            parse "(DEP-CSA4311-EGPD1923-ZZZZ-DEST/ESMS DOF/870601)" `shouldBe`
            Right
                (Dep
                     (DepartureContent
                          (fromJust (mkAircraftIdentification "CSA4311"))
                          Nothing
                          (fromJust (mkAerodrome "EGPD"))
                          (fromJust (mkHhmm 19 23))
                          (fromJust (mkAerodrome "ZZZZ"))
                          (emptyOtherInformation &
                           withDestination (fromJust (mkCodedDesignator "ESMS")) .
                           withDof (fromJust (mkDate 87 6 1)))))
