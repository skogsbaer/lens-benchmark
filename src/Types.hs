{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens
import qualified Data.Text as T
import LensUtil
import Control.DeepSeq
import GHC.Generics

data AllStores
    = AllStores
    { as_musicStore :: !MusicStore
    , as_grocery :: !T.Text
    }
    deriving (Eq, Show, Read, Generic, NFData)

data MusicStore
    = MusicStore
    { ms_name :: !T.Text
    , ms_gitAndBassDepartment :: !GitAndBassDepartment
    }
    deriving (Eq, Show, Read, Generic, NFData)

data GitAndBassDepartment
    = GitAndBassDepartment
    { gb_gitSubDepartment :: !GitDepartment
    , gb_bassSubDepartment :: !BassDepartment
    , gb_name :: !T.Text
    }
    deriving (Eq, Show, Read, Generic, NFData)

data GitDepartment = GitDepartment
    deriving (Eq, Show, Read, Generic, NFData)

data BassDepartment
    = BassDepartment
    { bd_name :: T.Text
    , bd_4string :: !Bass
    , bd_5string :: !Bass5
    }
    deriving (Eq, Show, Read, Generic, NFData)

data Bass5 = Bass5
    deriving (Eq, Show, Read, Generic, NFData)

data Bass
    = Bass
    { b_model :: !T.Text
    , b_body :: !BassBody
    }
    deriving (Eq, Show, Read, Generic, NFData)

data BassBody
    = BassBody
    { bb_color :: !T.Text
    , bb_electronics :: !BassElectronics
    }
    deriving (Eq, Show, Read, Generic, NFData)

data BassElectronics
    = BassElectronics
    { be_inputJack :: !InputJack
    , be_frontPickup :: !Pickup
    , be_middlePickup :: !Pickup
    , be_rearPickup :: !Pickup
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

$(makeLensesWith myLensRules ''AllStores)
$(makeLensesWith myLensRules ''PickupCover)
$(makeLensesWith myLensRules ''BassElectronics)
$(makeLensesWith myLensRules ''Bass)
$(makeLensesWith myLensRules ''BassDepartment)
$(makeLensesWith myLensRules ''GitDepartment)
$(makeLensesWith myLensRules ''GitAndBassDepartment)
$(makeLensesWith myLensRules ''MusicStore)
$(makeLensesWith myLensRules ''BassBody)
$(makeLensesWith myLensRules ''Pickup)
$(makeLensesWith myLensRules ''InputJack)
