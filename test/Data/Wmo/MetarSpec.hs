module Data.Wmo.MetarSpec
    ( spec
    ) where

import Data.Icao.Expected
import Data.Wmo.Metar
import Test.Hspec

-- | Most METARs retrieved from <https://aviationweather.gov/metar/data>
spec :: Spec
spec =
    describe "METAR paser" $ do
        it "parses (WMO) 'METAR ESMS 131250Z 18016KT 9999 -RA BKN008 12/12 Q0988'" $
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
            "parses (US) 'METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020'" $
            parse
                "METAR KJFK 151151Z 25004KT 10SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            Right
                (Metar
                     METAR
                     False
                     False
                     False
                     (mkAerodrome' "KJFK")
                     (mkDayTime' 15 11 51)
                     (Wind (Just (WindDirection 250)) (WindSpeedKt 4) Nothing Nothing)
                     False
                     (Just
                          (Visibility (VisibilityDistanceMile (Just 10) Nothing) Nothing Nothing []))
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)
        it
            "parses (US) 'METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020'" $
            parse
                "METAR KJFK 151151Z 25004KT 10 1/4SM FEW150 FEW250 22/18 A3003 RMK AO2 SLP169 T02170183 10217 20189 51020" `shouldBe`
            Right
                (Metar
                     METAR
                     False
                     False
                     False
                     (mkAerodrome' "KJFK")
                     (mkDayTime' 15 11 51)
                     (Wind (Just (WindDirection 250)) (WindSpeedKt 4) Nothing Nothing)
                     False
                     (Just
                          (Visibility
                               (VisibilityDistanceMile (Just 10) (Just (1, 4)))
                               Nothing
                               Nothing
                               []))
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)
