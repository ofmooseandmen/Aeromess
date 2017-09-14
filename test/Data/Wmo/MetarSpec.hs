module Data.Wmo.MetarSpec
    ( spec
    ) where

import Data.Icao.Expected
import Data.Wmo.Metar
import Test.Hspec

spec :: Spec
spec =
    describe "METAR paser" $
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
             (Wind (Just (WindDirection 180)) (Kt 16) Nothing Nothing)
             False
             (Just (Visibility (VisibilityDistance 9999) [] Nothing []))
             []
             []
             Nothing
             Nothing
             Nothing
             Nothing)
