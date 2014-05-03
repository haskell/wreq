{-# LANGUAGE BangPatterns, DeriveFunctor, RecordWildCards #-}

module Network.Wreq.Cache.Store
    (
      Store
    , empty
    , insert
    , lookup
    , fromList
    , toList
    ) where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List (foldl')
import Prelude hiding (lookup, map)
import qualified Data.HashMap.Lazy as HM
import qualified Data.PSQueue as PSQ

type Epoch = Int64

data Store k v = Store {
    capacity :: {-# UNPACK #-} !Int
  , size :: {-# UNPACK #-} !Int
  , epoch :: {-# UNPACK #-} !Epoch
  , lru :: !(PSQ.PSQ k Epoch)
  , map :: !(HM.HashMap k v)
  }

instance (Show k, Show v, Ord k, Hashable k) => Show (Store k v) where
    show st = "fromList " ++ show (toList st)

empty :: Ord k => Int -> Store k v
empty cap
  | cap <= 0  = error "empty: invalid capacity"
  | otherwise = Store cap 0 0 PSQ.empty HM.empty
{-# INLINABLE empty #-}

insert :: (Ord k, Hashable k) => k -> v -> Store k v -> Store k v
insert k v st@Store{..}
  | size < capacity || present =
    st { size  = if present then size else size + 1
       , epoch = epoch + 1
       , lru   = PSQ.insert k epoch lru
       , map   = HM.insert k v map
       }
  | otherwise =
      let Just (mink PSQ.:-> _, lru0) = PSQ.minView lru
      in st { epoch = epoch + 1
            , lru   = PSQ.insert k epoch lru0
            , map   = HM.insert k v $ if mink == k
                                      then map
                                      else HM.delete mink map
            }
  where present = k `HM.member` map
{-# INLINABLE insert #-}

lookup :: (Ord k, Hashable k) => k -> Store k v -> Maybe (v, Store k v)
lookup k st@Store{..} = do
  v <- HM.lookup k map
  let !st' = st { epoch = epoch + 1, lru = PSQ.insert k epoch lru }
  return (v, st')
{-# INLINABLE lookup #-}

fromList :: (Ord k, Hashable k) => Int -> [(k, v)] -> Store k v
fromList = foldl' (flip (uncurry insert)) . empty
{-# INLINABLE fromList #-}

toList :: (Ord k, Hashable k) => Store k v -> [(k, v)]
toList Store{..} = [(k,v) | (k PSQ.:-> _) <- PSQ.toList lru,
                            let v = map HM.! k]
{-# INLINABLE toList #-}
