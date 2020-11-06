{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import qualified Data.Text as T
import LensUtil
import Control.DeepSeq
import GHC.Generics

data Appointment
    = Appointment
    { app_title :: !T.Text
    , app_notification :: !Notification
    }
    deriving (Eq, Show, Read, Generic, NFData)

data Notification
    = Notification
    { not_title :: !T.Text
    , not_deliveredTo :: !MobileDevice
    }
    deriving (Eq, Show, Read, Generic, NFData)

data MobileDevice
    = MobileDevice
    { md_brand :: !T.Text
    , md_owner :: !Person
    }
    deriving (Eq, Show, Read, Generic, NFData)

data Person
    = Person
    { p_name :: !T.Text
    , p_guitar :: !Guitar
    }
    deriving (Eq, Show, Read, Generic, NFData)

data Address
    = Address
    { addr_city :: !T.Text
    , addr_zipCode :: !T.Text
    , addr_street :: !T.Text
    }
    deriving (Eq, Show, Read, Generic, NFData)

data Guitar
    = Guitar
    { g_model :: !T.Text
    , g_body :: !GuitarBody
    }
    deriving (Eq, Show, Read, Generic, NFData)

data GuitarBody
    = GuitarBody
    { gb_color :: !T.Text
    , gb_electronics :: !GuitarElectronics
    }
    deriving (Eq, Show, Read, Generic, NFData)

data GuitarElectronics
    = GuitarElectronics
    { ge_inputJack :: !InputJack
    , ge_frontPickup :: !Pickup
    , ge_middlePickup :: !Pickup
    , ge_rearPickup :: !Pickup
    }
    deriving (Eq, Show, Read, Generic, NFData)

data InputJack = InputJack
    deriving (Eq, Show, Read, Generic, NFData)

data Pickup
    = Pickup
    { p_cover :: !PickupCover
    , p_windings :: !Int
    }
    deriving (Eq, Show, Read, Generic, NFData)

data PickupCover
    = PickupCover
    { pc_color :: !T.Text }
    deriving (Eq, Show, Read, Generic, NFData)

$(makeLensesWith myLensRules ''PickupCover)
$(makeLensesWith myLensRules ''GuitarElectronics)
$(makeLensesWith myLensRules ''Guitar)
$(makeLensesWith myLensRules ''Address)
$(makeLensesWith myLensRules ''Person)
$(makeLensesWith myLensRules ''MobileDevice)
$(makeLensesWith myLensRules ''Appointment)
$(makeLensesWith myLensRules ''Notification)
$(makeLensesWith myLensRules ''GuitarBody)
$(makeLensesWith myLensRules ''Pickup)
$(makeLensesWith myLensRules ''InputJack)
