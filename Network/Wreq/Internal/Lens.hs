{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.Wreq.Internal.Lens
    ( HTTP.Request
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , requestVersion
    , onRequestBodyException
    , proxy
    , hostAddress
    , rawBody
    , decompress
    , redirectCount
    , responseTimeout
    , checkStatus
    , getConnectionWrapper
    , cookieJar
    , seshCookies
    , seshManager
    , seshRun
    -- * Useful functions
    , assoc
    , assoc2
    , setHeader
    , maybeSetHeader
    , deleteKey
    ) where

import qualified Data.ByteString as S
import           Data.List (partition)
import           Lens.Micro
import           Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types (HeaderName)
import           Network.Wreq.Internal.Types (Session)
import           Network.Wreq.Lens.Machinery (makeLenses)

---

makeLenses ''HTTP.Request
makeLenses ''Session

-- | `lookup` in Traversal form for an association list.
assoc :: Eq k => k -> Traversal' [(k, a)] a
assoc k f al = case lookup k al of
  Nothing -> pure al
  Just a  -> (\a' -> insert' a' al) <$> f a
    where insert' a' [] = [(k,a')]
          insert' a' ((k',x):xs) | k == k' = (k,a') : xs
                                 | otherwise = (k',x) : insert' a' xs

assoc2 :: Eq k => k -> Lens' [(k,a)] [a]
-- This is only a lens up to the ordering of the list (which changes
-- when we modify the list).
-- assoc2 :: (Eq b, Functor f) => b -> ([a] -> f [a]) -> [(b, a)] -> f [(b, a)]
assoc2 k f = fmap (uncurry ((++) . fmap ((,) k))) .
             _1 (f . fmap snd) . partition ((==k) . fst)

-- | Set a header to the given value, replacing any prior value.
setHeader :: HeaderName -> S.ByteString -> Request -> Request
setHeader name value = requestHeaders %~ ((name,value) :) . deleteKey name

-- | Set a header to the given value, but only if the header was not
-- already set.
maybeSetHeader :: HeaderName -> S.ByteString -> Request -> Request
maybeSetHeader name value = requestHeaders %~
  \hdrs -> case lookup name hdrs of
             Just _  -> hdrs
             Nothing -> (name,value) : hdrs

deleteKey :: (Eq a) => a -> [(a,b)] -> [(a,b)]
deleteKey key = filter ((/= key) . fst)
