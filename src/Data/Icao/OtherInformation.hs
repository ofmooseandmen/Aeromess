-- |
-- ICAO Field Type 18 - Other information.
-- This field is a collection of switches all optional, use the withXXXX function to
-- build a new instance starting from 'emptyOtherInformation'.
module Data.Icao.OtherInformation
    ( -- * Data
      OtherInformation(..)
    , PbnCapabilityCode(..)
    , SpecicalHandlingReason(..)
    , EstimatedElapsedTime(..)
    , SelCalCode
    -- * Builders
    , mkSelCalCode
    , emptyOtherInformation
    , withSpecialHandlingReason
    , withPbnCapabilities
    , withNavigationEquipments
    , withCommunicationEquipments
    , withDataCommunicationEquipments
    , withSurveillanceEquipments
    , withDeparture
    , withDestination
    , withDof
    , withRegistration
    , withEstimatedElapsedTimes
    , withSelCalCode
    ) where

import Data.Char
import Data.Icao.Lang
import Data.Icao.Location
import Data.Icao.Time
import Data.List
import Data.Maybe()

-- | Code describing PBN (Performance Base Navigation) capabilities.
-- Provides both RNAV (Area Navigation) and RNP (Required Navigation Performance) capabilities.
-- <https://en.wikipedia.org/wiki/Area_navigation>
-- <https://en.wikipedia.org/wiki/Required_navigation_performance>
data PbnCapabilityCode
    -- | RNAV
    = A1 -- ^ RNAV 10 (RNP 10)
    | B1 -- ^ RNAV 5 all permitted sensors
    | B2 -- ^ RNAV 5 GNSS
    | B4 -- ^ RNAV 5 VOR/DME
    | B3 -- ^ RNAV 5 DME/DME
    | B5 -- ^ RNAV 5 INS or IRS
    | B6 -- ^ RNAV 5 LORANC
    | C1 -- ^ RNAV 2 all permitted sensors
    | C2 -- ^ RNAV 2 GNSS
    | C3 -- ^ RNAV 2 DME/DME
    | C4 -- ^ RNAV 2 DME/DME/IRU
    | D1 -- ^ RNAV 1 all permitted sensors
    | D2 -- ^ RNAV 1 GNSS
    | D3 -- ^ RNAV 1 DME/DME
    | D4 -- ^ RNAV 1 DME/DME/IRU
    -- | RNP
    | L1 -- ^ RNP 4
    | O1 -- ^ Basic RNP 1 all permitted sensors
    | O2 -- ^ Basic RNP 1 GNSS
    | O3 -- ^ Basic RNP 1 DME/DME
    | O4 -- ^ Basic RNP 1 DME/DME/IRU
    | S1 -- ^ RNP APCH
    | S2 -- ^ RNP APCH with BAR-VNAV
    | T1 -- ^ RNP AR APCH with RF (special authorization required)
    | T2 -- ^ RNP AR APCH without RF (special authorization required
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Reason for special handling.
data SpecicalHandlingReason
    = ALTRV -- ^ for a flight operated in accordance with an altitude reservation.
    | ATFMX -- ^ for a flight approved for exemption from ATFM measures by the appropriate ATS authority.
    | FFR -- ^ fire-fighting.
    | FLTCK -- ^ flight check for calibration of navaids.
    | HAZMAT -- ^ for a flight carrying hazardous material.
    | HEAD -- ^ a flight with Head of State status.
    | HOSP -- ^ for a medical flight declared by medical authorities.
    | HUM -- ^ for a flight operating on a humanitarian mission.
    | MARSA -- ^ for a flight for which a military entity assumes responsibility for separation of military aircraft.
    | MEDEVAC -- ^ for a life critical medical emergency evacuation.
    | NONRVSM -- ^ for a non-RVSM capable flight intending to operate in RVSM airspace.
    | SAR -- ^ for a flight engaged in a search and rescue mission.
    | STATE -- ^ for a flight engaged in military, customs or police services.
    deriving (Bounded, Enum, Eq, Read, Show)

-- | A Significant point or FIR boundary designator and the accumulated estimated
-- elapsed time from take-off.
data EstimatedElapsedTime = EstimatedElapsedTime
    { location :: SignificantPoint -- ^ Significant point or FIR boundary designator
    , duration :: Hhmm -- time from take-off to location
    } deriving (Eq, Show)

-- | Selective radio calling (SELCAL or SelCal) Code.
-- The code is a sequence of four letters, written or transmitted as
-- an ordered two sets of two letters each (e.g., AB-CD).
-- Valid letter are A through S, excluding I, N and O. The letters within a given
-- pair are written or transmitted in alphabetical order (e.g., AB-CD is an allowable
-- distinct SELCAL code, as is CD-AB, but CD-BA is not).
-- A given letter can be used only once in a SELCAL code; letters may not be repeated
-- (e.g., AB-CD is allowable, but AA-BC and AB-BC are not).
newtype SelCalCode =
    SelCalCode String
    deriving (Eq, Show)

-- | Other information.
data OtherInformation = OtherInformation
    { specicalHandlingReason :: Maybe SpecicalHandlingReason -- ^ Reason for special handling by ATS, e.g. a search and rescue mission.
    , pbnCapabilities :: [PbnCapabilityCode] -- ^ RNAV and RNP capabilites.
    , navigationEquipments :: Maybe FreeText -- ^ Significant data related to navigation equipment, other than specified in PBN/,
                                             -- as required by the appropriate ATS authority. Indicate GNSS augmentation
                                             -- under this indicator, with a space between two or more methods of
                                             -- augmentation, e.g. NAV/GBAS SBAS.
    , communicationEquipments :: Maybe FreeText -- ^ Indicate communication equipment and capabilities not specified in Item 10 a).
    , dataCommunicationEquipments :: Maybe FreeText -- ^ Indicate data communication equipment and capabilities not specified in 10 a).
    , surveillanceEquipments :: Maybe FreeText -- ^ Indicate surveillance equipment and capabilities not specified in Item 10 b).
                                               -- Indicate as many RSP specification(s) as apply to the flight, using designator(s) with no space.
                                               -- Multiple RSP specifications are separated by a space. Example: RSP180 RSP400.
    , departure :: Maybe SignificantPoint -- ^ Name and location of departure aerodrome, if ZZZZ is inserted in Item 13, or the ATS unit from
                                          -- which supplementary flight plan data can be obtained, if AFIL is inserted in Item 13.
    , destination :: Maybe SignificantPoint -- ^ Name and location of destination aerodrome, if ZZZZ is inserted in Item 16.
    , dof :: Maybe Date -- ^ The date of flight departure in a six-figure format.
    , registration :: Maybe FreeText -- ^ The nationality or common mark and registration mark of the aircraft,
                                     -- if different from the aircraft identification in Item 7.
    , eets :: [EstimatedElapsedTime] -- Significant points or FIR boundary designators and accumulated estimated elapsed times
                                     -- from take-off to such points or FIR boundaries, when so prescribed on the basis of regional
                                     -- air navigation agreements, or by the appropriate ATS authority.
    , selCalCode :: Maybe SelCalCode -- ^ SELCAL Code, for aircraft so equipped.
    } deriving (Show, Eq)

allowedLetter :: Char -> Bool
allowedLetter l = l `elem` "ABCDEFGHJKLMPQRS"

isValidPair :: String -> Bool
isValidPair s = allowedLetter h && allowedLetter t && h < t
  where
    h = head s
    t = last s

-- | 'SelCalCode' smart constructor. Fails if given string is not a valid
-- SELCAL code.
mkSelCalCode :: (Monad m) => String -> m SelCalCode
mkSelCalCode s
    | isValid s = return (SelCalCode s)
    | otherwise = fail ("invalid SELCAL code=" ++ s)
  where
    isValid :: String -> Bool
    isValid c =
        all isUpper c && -- all upper
        length c == 4 && -- 4 char
        length (nub c) == 4 && -- no duplicate
        isValidPair (take 2 c) && -- first pair is valid
        isValidPair (drop 2 c) -- second pair is valid

-- | Returns empty 'OtherInformation'.
emptyOtherInformation :: OtherInformation
emptyOtherInformation =
    OtherInformation
        Nothing
        []
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        Nothing

-- | Sets the reason for special handling.
withSpecialHandlingReason :: SpecicalHandlingReason -> OtherInformation -> OtherInformation
withSpecialHandlingReason s oi = oi {specicalHandlingReason = Just s}

-- | Sets the performance based capabilities.
withPbnCapabilities :: [PbnCapabilityCode] -> OtherInformation -> OtherInformation
withPbnCapabilities p oi = oi {pbnCapabilities = p}

-- | Sets the navigation equipments.
withNavigationEquipments :: FreeText -> OtherInformation -> OtherInformation
withNavigationEquipments n oi = oi {navigationEquipments = Just n}

-- | Sets the communication equipments.
withCommunicationEquipments :: FreeText -> OtherInformation -> OtherInformation
withCommunicationEquipments c oi = oi {navigationEquipments = Just c}

-- | Sets the data communication equipments.
withDataCommunicationEquipments :: FreeText -> OtherInformation -> OtherInformation
withDataCommunicationEquipments d oi = oi {navigationEquipments = Just d}

-- | Sets the surveillance equipments.
withSurveillanceEquipments :: FreeText -> OtherInformation -> OtherInformation
withSurveillanceEquipments s oi = oi {navigationEquipments = Just s}

-- | Sets the departure location.
withDeparture :: SignificantPoint -> OtherInformation -> OtherInformation
withDeparture d oi = oi {departure = Just d}

-- | Sets the destination location.
withDestination :: SignificantPoint -> OtherInformation -> OtherInformation
withDestination d oi = oi {destination = Just d}

-- | Sets the date of flight.
withDof :: Date -> OtherInformation -> OtherInformation
withDof d oi = oi {dof = Just d}

-- | Sets the aircraft registration.
withRegistration :: FreeText -> OtherInformation -> OtherInformation
withRegistration r oi = oi {registration = Just r}

-- | Sets the accumulated estimated elapsed times to location and/or FIR boundaries.
withEstimatedElapsedTimes :: [EstimatedElapsedTime] -> OtherInformation -> OtherInformation
withEstimatedElapsedTimes e oi = oi {eets = e}

-- | Sets the SELCAL code.
withSelCalCode :: SelCalCode -> OtherInformation -> OtherInformation
withSelCalCode c oi = oi {selCalCode = Just c}
