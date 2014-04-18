module Network.WReq.Lens.Machinery
    (
      makeLenses
    , toCamelCase
    ) where

import Control.Lens ((&), (.~))
import Control.Lens.TH (defaultRules, lensField, makeLensesWith)
import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (Dec, Name, Q)

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ Just)

toCamelCase :: String -> String
toCamelCase (x0:x0s)  = x0 : go x0s
  where go ('_':x:xs) = toUpper x : go xs
        go (x:xs)     = x : go xs
        go []         = []
toCamelCase []        = []
