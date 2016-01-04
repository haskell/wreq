{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wreq.Lens.Extra
       ( folding
       , (?~)
       ) where

import Data.Foldable (traverse_)
import Data.Functor.Contravariant (phantom)
import Lens.Micro hiding (folding)
import Lens.Micro.Contra (Fold)
       
--- 

-- | As defined in `Control.Lens.Fold`.
--
-- Obtain a `Fold` by lifting an operation that returns a `Foldable` result.
folding :: Foldable f => (s -> f a) -> Fold s a
folding sfa agb = phantom . traverse_ agb . sfa

-- | As defined in `Control.Lens.Setter`.
--
-- Set the target of a `Lens`, `Traversal` or `Setter` to `Just` a value.
(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ t = set l $ Just t
