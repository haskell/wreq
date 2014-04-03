module Network.WReq.Lens.Machinery
    (
      makeLenses
    ) where

import Control.Lens ((&), (.~))
import Control.Lens.TH (defaultRules, lensField, makeLensesWith)
import Language.Haskell.TH.Syntax (Dec, Name, Q)

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (defaultRules & lensField .~ Just)
