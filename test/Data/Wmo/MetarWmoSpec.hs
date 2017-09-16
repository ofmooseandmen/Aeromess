module Data.Wmo.MetarWmoSpec
    ( spec
    ) where

import Data.Icao.Expected
import Data.Wmo.Metar
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "WMO METAR paser" $ do
        it "parses 'LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG'" $
            parse "METAR LFPG 152330Z 25003KT CAVOK 10/09 Q1012 NOSIG" `shouldBe`
            Right
                (Metar
                     METAR
                     False
                     False
                     False
                     (mkAerodrome' "LFPG")
                     (mkDayTime' 15 23 30)
                     (Wind (Just (WindDirection 250)) (WindSpeedKt 3) Nothing Nothing)
                     True
                     Nothing
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)
        it "parses 'METAR ESMS 131250Z 18016KT 9999 -RA BKN008 12/12 Q0988'" $
            parse "METAR ESMS 131250Z 18016KT 9999 -RA BKN008 12/12 Q0988" `shouldBe`
            Right
                (Metar
                     METAR
                     False
                     False
                     False
                     (mkAerodrome' "ESMS")
                     (mkDayTime' 13 12 50)
                     (Wind (Just (WindDirection 180)) (WindSpeedKt 16) Nothing Nothing)
                     False
                     (Just (Visibility (VisibilityDistanceMetre 9999) Nothing Nothing []))
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)
        it
            "parses 'METAR LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N R22/P1500U +SN BKN022 OVC050 M04/M07 Q1020 NOSIG 8849//91'" $
            parse
                "METAR LBBG 041600Z 12012MPS 090V150 1400 R04/P1500N R22/P1500U +SN BKN022 OVC050 M04/M07 Q1020 NOSIG 8849//91" `shouldBe`
            Right
                (Metar
                     METAR
                     False
                     False
                     False
                     (mkAerodrome' "LBBG")
                     (mkDayTime' 4 16 0)
                     (Wind
                          (Just (WindDirection 120))
                          (WindSpeedMps 12)
                          Nothing
                          (Just (VariableDirection (WindDirection 90) (WindDirection 150))))
                     False
                     (Just
                          (Visibility
                               (VisibilityDistanceMetre 1400)
                               Nothing
                               Nothing
                               [ (RunwayVisualRange
                                      (RunwayDesignator "04")
                                      (VisibilityDistanceMetre 1500)
                                      (Just Higher)
                                      (Just NoChange))
                               , (RunwayVisualRange
                                      (RunwayDesignator "22")
                                      (VisibilityDistanceMetre 1500)
                                      (Just Higher)
                                      (Just Up))
                               ]))
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)
