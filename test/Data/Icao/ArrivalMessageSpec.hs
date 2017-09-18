module Data.Icao.ArrivalMessageSpec
    ( spec
    ) where

import Data.Maybe(fromJust)
import Data.Icao.AtsMessage
import Test.Hspec

spec :: Spec
spec =
    describe "Arrival (ARR) message parser" $ do
        it "parses an ARR message containing only the mandatory fields" $
            parse "(ARR-CSA406-LHBP0800-LKPR0913)" `shouldBe`
            Right
                (Arr
                     (ArrivalContent
                          (fromJust (mkAircraftIdentification "CSA406"))
                          Nothing
                          (fromJust (mkAerodrome "LHBP"))
                          (fromJust (mkHhmm 8 0))
                          Nothing
                          (fromJust (mkAerodrome "LKPR"))
                          (fromJust (mkHhmm 9 13))
                          Nothing))
        it "parses an ARR message containing the SSR mode and code" $
            parse "(ARR-SAS2000/A1001-ESMS0800-ESGG0905)" `shouldBe`
            Right
                (Arr
                     (ArrivalContent
                          (fromJust (mkAircraftIdentification "SAS2000"))
                          (Just (fromJust (mkSsrCode "1001")))
                          (fromJust (mkAerodrome "ESMS"))
                          (fromJust (mkHhmm 8 0))
                          Nothing
                          (fromJust (mkAerodrome "ESGG"))
                          (fromJust (mkHhmm 9 5))
                          Nothing))
        it "parses an ARR message indicating diversionary landing" $
            parse "(ARR-AFR154-LFPO0803-ELLX-LFJL0920)" `shouldBe`
            Right
                (Arr
                     (ArrivalContent
                          (fromJust (mkAircraftIdentification "AFR154"))
                          Nothing
                          (fromJust (mkAerodrome "LFPO"))
                          (fromJust (mkHhmm 8 3))
                          (Just (fromJust (mkAerodrome "ELLX")))
                          (fromJust (mkAerodrome "LFJL"))
                          (fromJust (mkHhmm 9 20))
                          Nothing))
        it "parses an ARR message containing ADAR ZZZZ and the name of the destination" $
            parse "(ARR-AFR154-LFPO0803-ELLX-ZZZZ0920 SOMEWHERE NORTH OF EPINAL)" `shouldBe`
            Right
                (Arr
                     (ArrivalContent
                          (fromJust (mkAircraftIdentification "AFR154"))
                          Nothing
                          (fromJust (mkAerodrome "LFPO"))
                          (fromJust (mkHhmm 8 3))
                          (Just (fromJust (mkAerodrome "ELLX")))
                          (fromJust (mkAerodrome "ZZZZ"))
                          (fromJust (mkHhmm 9 20))
                          (Just (fromJust (mkFreeText "SOMEWHERE NORTH OF EPINAL")))))
