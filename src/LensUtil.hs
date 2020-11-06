module LensUtil where

import Control.Lens
import Language.Haskell.TH

myLensRules :: LensRules
myLensRules = lensRules & lensField .~ (\_tyName _ s -> [TopName (mkName (nameBase s ++ "L"))])
