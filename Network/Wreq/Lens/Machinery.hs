module Network.Wreq.Lens.Machinery
    (
      makeLenses
    , fieldName
    , toCamelCase
    ) where

import Control.Lens ((&), (.~))
import Control.Lens.TH hiding (makeLenses)
import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (Dec, Name, Q, mkName, nameBase)

defaultRules :: LensRules
defaultRules = lensRules

fieldName :: (String -> String) -> [Name] -> Name -> [DefName]
fieldName f _ name = [TopName . mkName . f . nameBase $ name]

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ fieldName id)

toCamelCase :: String -> String
toCamelCase (x0:x0s)  = x0 : go x0s
  where go ('_':x:xs) = toUpper x : go xs
        go (x:xs)     = x : go xs
        go []         = []
toCamelCase []        = []
