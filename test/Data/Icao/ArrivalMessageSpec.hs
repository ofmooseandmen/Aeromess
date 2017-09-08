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
            parse parser "(ARR-CSA406-LHBP0800-LKPR0913)" `shouldBe`
            Right (ArrivalMessage
                (mkAircraftIdentification' "CSA406")
                Nothing
                Nothing
                (mkAerodromeName' "LHBP")
                (mkHhmm' 8 0)
                Nothing
                (mkAerodromeName' "LKPR")
                (mkHhmm' 9 13)
                Nothing)
        it "parses an ARR message containing the SSR mode and code" $
            parse parser "(ARR-SAS2000/A1001-ESMS0800-ESGG0905)" `shouldBe`
            Right (ArrivalMessage
                (mkAircraftIdentification' "SAS2000")
                (Just A)
                (Just (mkSsrCode' "1001"))
                (mkAerodromeName' "ESMS")
                (mkHhmm' 8 0)
                Nothing
                (mkAerodromeName' "ESGG")
                (mkHhmm' 9 5)
                Nothing)
        it "parses an ARR message indicating diversionary landing" $
            parse parser "(ARR-AFR154-LFPO0803-ELLX-LFJL0920)" `shouldBe`
            Right (ArrivalMessage
                (mkAircraftIdentification' "AFR154")
                Nothing
                Nothing
                (mkAerodromeName' "LFPO")
                (mkHhmm' 8 3)
                (Just (mkAerodromeName' "ELLX"))
                (mkAerodromeName' "LFJL")
                (mkHhmm' 9 20)
                Nothing)
        it
            "parses an ARR message containing ADAR ZZZZ and the name of the destination" $
            parse parser
                "(ARR-AFR154-LFPO0803-ELLX-ZZZZ0920 SOMEWHERE NORTH OF EPINAL)" `shouldBe`
            Right (ArrivalMessage
                (mkAircraftIdentification' "AFR154")
                Nothing
                Nothing
                (mkAerodromeName' "LFPO")
                (mkHhmm' 8 3)
                (Just (mkAerodromeName' "ELLX"))
                (mkAerodromeName' "ZZZZ")
                (mkHhmm' 9 20)
                (Just "SOMEWHERE NORTH OF EPINAL"))
