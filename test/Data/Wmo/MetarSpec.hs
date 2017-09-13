module Data.Wmo.MetarSpec
    ( spec
    ) where

import Data.Icao.Expected
import Data.Maybe
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
                     (Wind (Just (mkDegrees' 180)) (mkNatural2' 16) Nothing KT Nothing)
                     False
                     (Just (Visibility (mkNatural4' 9999) [] Nothing []))
                     []
                     []
                     Nothing
                     Nothing
                     Nothing
                     Nothing)

mkDegrees' :: Int -> Degrees
mkDegrees' d = fromMaybe (error "invalid degrees") (mkDegrees d)
