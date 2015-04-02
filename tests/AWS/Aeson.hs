{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AWS.Aeson
    (
      object
    , string
    , true
    , (.=)
    ) where

import Data.Aeson hiding ((.=))
import Data.Text (Text, pack)
import GHC.Exts
import qualified Data.Vector as Vector

instance Num Value where
    fromInteger = Number . fromInteger

instance Fractional Value where
    fromRational = Number . fromRational

instance IsList Value where
    type Item Value  = Value
    fromList         = Array . Vector.fromList
    toList (Array a) = Vector.toList a
    toList _         = error "AWS.Aeson.toList"

class Stringy a where
    string :: a -> Value

instance Stringy Text where
    string = String

instance Stringy String where
    string = String . pack

true :: Value
true = Bool True

(.=) :: Text -> Value -> (Text, Value)
a .= b = (a,b)
