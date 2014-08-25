{-# LANGUAGE CPP #-}

module Network.Wreq.Lens.Machinery
    (
      makeLenses
    , toCamelCase
    ) where

import Control.Lens ((&), (.~))
import Control.Lens.TH hiding (makeLenses)
import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (Dec, Name, Q, mkName, nameBase)

#if MIN_VERSION_lens(4,4,0)
defaultRules :: LensRules
defaultRules = lensRules

fieldName :: [Name] -> Name -> [DefName]
fieldName _ name = [TopName (mkName (nameBase name))]
#else
fieldName :: String -> Maybe String
fieldName = Just
#endif

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ fieldName)

toCamelCase :: String -> String
toCamelCase (x0:x0s)  = x0 : go x0s
  where go ('_':x:xs) = toUpper x : go xs
        go (x:xs)     = x : go xs
        go []         = []
toCamelCase []        = []
