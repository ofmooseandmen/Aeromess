-- |
-- ICAO Field Type 19 - Supplementary information.
-- This field is a collection of switches all optional, use the withXXXX function to
-- build a new instance starting from 'emptySupplementaryInformation'.
module Data.Icao.SupplementaryInformation
    (
    -- * Data
      PersonsOnBoard
    , Transmitter(..)
    , SurvivalEquipment(..)
    , LifeJacket(..)
    , Dinghies(number, totalCapacity, covered, colour)
    , SupplementaryInformation(..)
    -- * Builders
    , emptySupplementaryInformation
    , withFuelEndurance
    , withPersonsOnBoard
    , witAvailableTransmitters
    , withSurvivalEquipments
    , withAircraftDescription
    , withLifeJackets
    , withDinghies
    , withOtherRemarks
    , withPilotInCommand
    -- * Smart constructors
    , mkPersonsOnBoard
    , mkDinghies
    ) where

import Data.Char (isUpper)
import Data.Icao.Lang
import Data.Icao.Time
import Data.Maybe ()

-- | Persons on board the aircraft.
newtype PersonsOnBoard =
    PersonsOnBoard Int
    deriving (Eq, Show)

-- | Transmitter.
data Transmitter
    = UHF -- ^ frequency 243.0 MHz (UHF).
    | VHF -- ^ frequency 121.5 MHz (VHF).
    | ELT -- ^ locator transmitter (ELT).
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Survival equipment.
data SurvivalEquipment
    = Polar -- ^  polar survival equipment.
    | Desert -- ^  desert survival equipment.
    | Maritime -- ^  maritime survival equipment.
    | Jungle -- ^ jungle survival equipment.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Life Jacket.
data LifeJacket
    = WithLight -- ^ life jacket equipped with lights.
    | WithFluorescein -- ^ life jacke equipped with fluorescein.
    | WithRadioUHF -- ^ life jacket radio equipped with UHF on frequency 243.0 MHz.
    | WithRadioVHF -- ^ life jacket radio is equipped with VHF on frequency 121.5 MHz.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Dinghies.
data Dinghies = Dinghies
    { number :: Maybe Int -- ^ the number of dinghies carried
    , totalCapacity :: Maybe Int -- ^  the total capacity, in persons carried, of all dinghies.
    , covered :: Bool -- ^ whether dinghies are covered.
    , colour :: Maybe String -- ^ the colour of the dinghies.
    } deriving (Eq, Show)

-- | Supplementary information data.
data SupplementaryInformation = SupplementaryInformation
    { fuelEndurance :: Maybe Hhmm -- ^ the fuel endurance in hours and minutes.
    , personsOnBoard :: Maybe PersonsOnBoard -- ^ the total number of persons on board, when so prescribed by the appropriate ATS authority.
    , availableTransmitters :: [Transmitter] -- ^ special transmitter(s).
    , survivalEquipments :: [SurvivalEquipment] -- ^ survival equipment(s) carried on board.
    , lifeJackets :: [LifeJacket] -- ^ type of life jackets carried on board.
    , dinghies :: Dinghies -- ^ description of the dinghies caried on board.
    , aircraftDescription :: Maybe FreeText -- ^ the colour of the aircraft and any Significant markings (this may include the aircraft registration).
    , otherRemarks :: Maybe FreeText -- ^ plain language indicating any other survival equipment carried and any other useful remarks.
    , pilotInCommand :: Maybe FreeText -- ^ the name of the pilot-in-command and possibly the contact phone.
    } deriving (Eq, Show)

-- | Returns empty 'SupplementaryInformation'.
emptySupplementaryInformation :: SupplementaryInformation
emptySupplementaryInformation =
    SupplementaryInformation
        Nothing
        Nothing
        []
        []
        []
        (Dinghies Nothing Nothing False Nothing)
        Nothing
        Nothing
        Nothing

-- | Sets the aircraft fuel endurance.
withFuelEndurance :: Hhmm -> SupplementaryInformation -> SupplementaryInformation
withFuelEndurance fe si = si {fuelEndurance = Just fe}

-- | Sets the number of persons on board the aircraft.
withPersonsOnBoard :: PersonsOnBoard -> SupplementaryInformation -> SupplementaryInformation
withPersonsOnBoard n si = si {personsOnBoard = Just n}

-- | Sets the available transmitters on board the aircraft.
witAvailableTransmitters :: [Transmitter] -> SupplementaryInformation -> SupplementaryInformation
witAvailableTransmitters t si = si {availableTransmitters = t}

-- | Sets the available survival equipements on board the aircraft.
withSurvivalEquipments :: [SurvivalEquipment]
                       -> SupplementaryInformation
                       -> SupplementaryInformation
withSurvivalEquipments s si = si {survivalEquipments = s}

-- | Sets the aircraft description.
withAircraftDescription :: FreeText -> SupplementaryInformation -> SupplementaryInformation
withAircraftDescription d si = si {aircraftDescription = Just d}

-- | Sets the detail of the pilot in command.
withPilotInCommand :: FreeText -> SupplementaryInformation -> SupplementaryInformation
withPilotInCommand p si = si {pilotInCommand = Just p}

-- | Sets the dinghies.
withDinghies :: Dinghies -> SupplementaryInformation -> SupplementaryInformation
withDinghies d si = si {dinghies = d}

-- | Sets the details of the life jackets available on board the aircraft.
withLifeJackets :: [LifeJacket] -> SupplementaryInformation -> SupplementaryInformation
withLifeJackets l si = si {lifeJackets = l}

-- | Sets the othe remarks.
withOtherRemarks :: FreeText -> SupplementaryInformation -> SupplementaryInformation
withOtherRemarks r si = si {pilotInCommand = Just r}

-- | 'PersonsOnBoard' smart constructor. Fails if given number is not in range [1 .. 999].
mkPersonsOnBoard
    :: (Monad m)
    => Int -> m PersonsOnBoard
mkPersonsOnBoard n
    | n < 1 || n > 999 = fail ("invalid persons on board=" ++ show n)
    | otherwise = return (PersonsOnBoard n)

mkDinghies
    :: (Monad m)
    => Maybe Int -> Maybe Int -> Bool -> Maybe String -> m Dinghies
mkDinghies nb cap cov col
    | maybe False (< 0) nb || maybe False (> 99) nb =
        fail ("invalid number of dinghies=" ++ show nb)
    | maybe False (< 0) cap || maybe False (> 999) cap =
        fail ("invalid total capacity of dinghies=" ++ show cap)
    | maybe False (not . all isUpper) col = fail ("invalid dinghies color=" ++ show col)
    | otherwise = return (Dinghies nb cap cov col)
