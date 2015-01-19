{-# LANGUAGE BangPatterns, DeriveFunctor, RecordWildCards #-}

module Network.Wreq.Cache.Store
    (
      Store
    , empty
    , insert
    , delete
    , lookup
    , fromList
    , toList
    ) where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List (foldl')
import Prelude hiding (lookup, map)
import qualified Data.HashPSQ as HashPSQ

type Epoch = Int64

data Store k v = Store {
    capacity :: {-# UNPACK #-} !Int
  , size     :: {-# UNPACK #-} !Int
  , epoch    :: {-# UNPACK #-} !Epoch
  , psq      :: !(HashPSQ.HashPSQ k Epoch v)
  }

instance (Show k, Show v, Ord k, Hashable k) => Show (Store k v) where
    show st = "fromList " ++ show (toList st)

empty :: Ord k => Int -> Store k v
empty cap
  | cap <= 0  = error "empty: invalid capacity"
  | otherwise = Store cap 0 0 HashPSQ.empty
{-# INLINABLE empty #-}

insert :: (Ord k, Hashable k) => k -> v -> Store k v -> Store k v
insert k v st@Store{..} = case HashPSQ.insertView k epoch v psq of
  (Just (_, _), psq0) -> st {epoch = epoch + 1, psq = psq0}
  (Nothing,     psq0)
    | size < capacity -> st {size = size + 1, epoch = epoch + 1, psq = psq0}
    | otherwise       -> st {epoch = epoch + 1, psq = HashPSQ.deleteMin psq0}
{-# INLINABLE insert #-}

lookup :: (Ord k, Hashable k) => k -> Store k v -> Maybe (v, Store k v)
lookup k st@Store{..} = case HashPSQ.alter tick k psq of
  (Nothing, _)   -> Nothing
  (Just v, psq0) -> Just (v, st { epoch = epoch + 1, psq = psq0 })
  where tick Nothing       = (Nothing, Nothing)
        tick (Just (_, v)) = (Just v, Just (epoch, v))
{-# INLINABLE lookup #-}

delete :: (Ord k, Hashable k) => k -> Store k v -> Store k v
delete k st@Store{..} = case HashPSQ.deleteView k psq of
  Nothing           -> st
  Just (_, _, psq0) -> st {size = size - 1, psq = psq0}
{-# INLINABLE delete #-}

fromList :: (Ord k, Hashable k) => Int -> [(k, v)] -> Store k v
fromList = foldl' (flip (uncurry insert)) . empty
{-# INLINABLE fromList #-}

toList :: (Ord k, Hashable k) => Store k v -> [(k, v)]
toList Store{..} = [(k,v) | (k, _, v) <- HashPSQ.toList psq]
{-# INLINABLE toList #-}
