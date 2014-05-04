{-# LANGUAGE RecordWildCards #-}

module Properties.Store
    (
      tests
    ) where

import Data.Functor ((<$>))
import Data.Hashable (Hashable)
import Data.List (foldl', sort, sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Network.Wreq.Cache.Store as S
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Positive(..), Small(..))

data StoreModel k v = StoreModel {
    smCap :: Int
  , smGen :: Int
  , smSize :: Int
  , smList :: [(k,v,Int)]
  } deriving (Show)

emptySM :: Int -> StoreModel k v
emptySM n = StoreModel n 0 0 []

insertSM :: Eq k => k -> v -> StoreModel k v -> StoreModel k v
insertSM k v sm@StoreModel{..}
  | smSize < smCap || present =
    sm { smGen = smGen + 1
       , smSize = if present then smSize else smSize + 1
       , smList = (k,v,smGen) : [x | x@(kk,_,_) <- smList, kk /= k]
       }
  | otherwise =
    sm { smGen = smGen + 1
       , smList = (k,v,smGen) : tail (sortBy (comparing $ \(_,_,g) -> g) smList)
       }
  where present = any (\(kk,_,_) -> k == kk) smList

lookupSM :: Eq k => k -> StoreModel k v -> Maybe (v, StoreModel k v)
lookupSM k sm@StoreModel{..} = listToMaybe
                               [(v, sm') | (kk,v,_) <- smList, k == kk]
  where sm' = sm { smGen = smGen + 1
                 , smList = [(kk,v,if kk == k then smGen else g)
                            | (kk,v,g) <- smList]
                 }

fromListSM :: Eq k => Int -> [(k,v)] -> StoreModel k v
fromListSM = foldl' (flip (uncurry insertSM)) . emptySM

toListSM :: StoreModel k v -> [(k,v)]
toListSM sm = [(k,v) | (k,v,_) <- smList sm]

unS :: (Ord k, Hashable k, Ord v) => S.Store k v -> [(k,v)]
unS = sort . S.toList

unM :: (Ord k, Ord v) => StoreModel k v -> [(k,v)]
unM = sort . toListSM

type N = Positive (Small Int)

unN :: N -> Int
unN (Positive (Small n)) = n

t_fromList :: N -> [(Char,Char)] -> Bool
t_fromList n xs = unS (S.fromList (unN n) xs) == unM (fromListSM (unN n) xs)

t_lookup :: N -> Char -> [(Char,Char)] -> Bool
t_lookup n k xs = (fmap unS <$> S.lookup k s) == (fmap unM <$> lookupSM k m)
  where
    s = S.fromList (unN n) xs
    m = fromListSM (unN n) xs

t_lookup1 :: N -> Char -> Char -> [(Char, Char)] -> Bool
t_lookup1 n k v xs = t_lookup n k ((k,v):xs)

tests :: [Test]
tests = [
    testProperty "t_fromList" t_fromList
  , testProperty "t_lookup" t_lookup
  , testProperty "t_lookup1" t_lookup1
  ]
