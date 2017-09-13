-- | Field Type 10 - Equipment and capabilities.
module Data.Icao.F10
    ( ComNavAppCapabilityCode(..)
    , SurveillanceCapabilityCode(..)
    ) where

-- | Code describing radiocommunication, navigation and approach aid equipment and capabilities.
data ComNavAppCapabilityCode
    = N -- ^ no COM/NAV/approach aid equipment for the route to be flown is carried, or the equipment is unserviceable
    | S -- ^ Standard COM/NAV/approach aid equipment for the route to be flown is carried and serviceable
    | A -- ^ GBAS landing system
    | B -- ^ PLV (APV with SBAS)
    | C -- ^ LORAN C
    | D -- ^ DME
    | E1 -- ^ FMC WPR ACARS
    | E2 -- ^ D-FIX ACARS
    | E3 -- ^ PDC ACARS
    | F -- ^ ADF
    | G -- ^ GNSS
    | H -- ^ HF RTF
    | I -- ^ Intertial navaigation
    | J1 -- ^ CPDLC ATN VDL Mode 2
    | J2 -- ^ CPDLC FANS 1/A HFDL
    | J3 -- ^ CPDLC FANS 1/A VDL Mode A
    | J4 -- ^ CPDLC FANS 1/A VDL Mode 2
    | J5 -- ^ CPDLC FANS 1/A SATCOM (INMARSAT)
    | J6 -- ^ CPDLC FANS 1/A SATCOM (MTSAT)
    | J7 -- ^ CPDLC FANS 1/A SATCOM (Iridium)
    | K -- ^ MLS
    | L -- ^ ILS
    | M1 -- ^ ATC SATVOICE (INMARSAT)
    | M2 -- ^ ATC SATVOICE (MTSAT)
    | M3 -- ^ ATC SATVOICE (Iridium)
    | O -- ^ VOR
    | P1 -- ^ CPDLC RCP 400
    | P2 -- ^ CPDLC RCP 240
    | P3 -- ^ SATVOICE RCP 400
    | P4 -- ^ Reserved for RCP
    | P5 -- ^ Reserved for RCP
    | P6 -- ^ Reserved for RCP
    | P7 -- ^ Reserved for RCP
    | P8 -- ^ Reserved for RCP
    | P9 -- ^ Reserved for RCP
    | R -- ^ PBN approved
    | T -- ^ TACAN
    | U -- ^ UHF RTF
    | V -- ^ VHF RTF
    | W -- ^ RVSM approved
    | X -- ^ MNPS approved
    | Y -- ^ VHF with 8.33 kHz channel spacing capability
    | Z -- ^ Other equipment carried or other capabilities
    deriving (Bounded, Enum, Eq, Read, Show)

-- | Code describing surveillance equipment and capabilities.
data SurveillanceCapabilityCode
    = NONE -- ^ no surveillance equipment for the route to be flown is carried, or the equipment is unserviceable
    | MODE_A -- ^ Transponder - Mode A (4 digits — 4,096 codes)
    | MODE_C -- ^ Transponder - Mode A (4 digits — 4,096 codes) and Mode C
    | MODE_S_E -- ^ Transponder - Mode S, including aircraft identification, pressure-altitude and extended squitter (ADS-B) capability
    | MODE_S_H -- ^ Transponder - Mode S, including aircraft identification, pressure-altitude and enhanced surveillance capability
    | MODE_S_I -- ^ Transponder - Mode S, including aircraft identification, but no pressure-altitude capability
    | MODE_S_L -- ^ Transponder - Mode S, including aircraft identification, pressure-altitude, extended squitter (ADS-B) and enhanced surveillance capability
    | MODE_S_P -- ^ Transponder - Mode S, including pressure-altitude, but no aircraft identification capability
    | MODE_S -- ^ Transponder - Mode S, including both pressure-altitude and aircraft identification capability
    | MODE_S_X -- ^ Transponder - Mode S with neither aircraft identification nor pressure-altitude capability
    deriving (Bounded, Enum, Eq, Read, Show)
