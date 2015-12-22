{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wreq.Lens.Machinery
    ( makeLenses
    , fieldName
    , toCamelCase
    ) where

import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (Dec, Name, Q, mkName, nameBase)
import Lens.Micro ((&), (.~))
import Lens.Micro.TH hiding (makeLenses)

---

defaultRules :: LensRules
defaultRules = lensRules

fieldName :: (String -> String) -> Name -> [Name] -> Name -> [DefName]
fieldName f _ _ name = [TopName . mkName . f . nameBase $ name]

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ fieldName id)

toCamelCase :: String -> String
toCamelCase []        = []
toCamelCase (x0:x0s)  = x0 : go x0s
  where go ('_':x:xs) = toUpper x : go xs
        go (x:xs)     = x : go xs
        go []         = []
