module Data.Icao.ArrivalMessageSpec
    ( spec
    ) where

import Data.Icao.AtsMessage
import Data.Icao.Expected
import Test.Hspec

spec :: Spec
spec =
    describe "Arrival (ARR) message parser" $ do
        it "parses an ARR message containing only the mandatory fields" $
            parse "(ARR-CSA406-LHBP0800-LKPR0913)" `shouldBe`
            Right
                (ArrivalMessage
                     (mkAircraftIdentification' "CSA406")
                     Nothing
                     (mkAerodrome' "LHBP")
                     (mkHhmm' 8 0)
                     Nothing
                     (mkAerodrome' "LKPR")
                     (mkHhmm' 9 13)
                     Nothing)
        it "parses an ARR message containing the SSR mode and code" $
            parse "(ARR-SAS2000/A1001-ESMS0800-ESGG0905)" `shouldBe`
            Right
                (ArrivalMessage
                     (mkAircraftIdentification' "SAS2000")
                     (Just (mkSsrCode' "1001"))
                     (mkAerodrome' "ESMS")
                     (mkHhmm' 8 0)
                     Nothing
                     (mkAerodrome' "ESGG")
                     (mkHhmm' 9 5)
                     Nothing)
        it "parses an ARR message indicating diversionary landing" $
            parse "(ARR-AFR154-LFPO0803-ELLX-LFJL0920)" `shouldBe`
            Right
                (ArrivalMessage
                     (mkAircraftIdentification' "AFR154")
                     Nothing
                     (mkAerodrome' "LFPO")
                     (mkHhmm' 8 3)
                     (Just (mkAerodrome' "ELLX"))
                     (mkAerodrome' "LFJL")
                     (mkHhmm' 9 20)
                     Nothing)
        it "parses an ARR message containing ADAR ZZZZ and the name of the destination" $
            parse "(ARR-AFR154-LFPO0803-ELLX-ZZZZ0920 SOMEWHERE NORTH OF EPINAL)" `shouldBe`
            Right
                (ArrivalMessage
                     (mkAircraftIdentification' "AFR154")
                     Nothing
                     (mkAerodrome' "LFPO")
                     (mkHhmm' 8 3)
                     (Just (mkAerodrome' "ELLX"))
                     (mkAerodrome' "ZZZZ")
                     (mkHhmm' 9 20)
                     (Just (mkFreeText' "SOMEWHERE NORTH OF EPINAL")))
