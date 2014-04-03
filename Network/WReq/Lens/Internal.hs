{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.WReq.Lens.Internal
    (
      HTTP.Request
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , hostAddress
    , rawBody
    , decompress
    , redirectCount
    , responseTimeout
    , checkStatus
    , getConnectionWrapper
    , cookieJar
    -- * Useful functions
    , assoc
    , assoc2
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (_1)
import Data.List (partition)
import Network.WReq.Lens.Machinery (makeLenses)
import qualified Network.HTTP.Client as HTTP

makeLenses ''HTTP.Request

assoc :: (Eq k, Applicative f) => k -> (a -> f a) -> [(k, a)] -> f [(k, a)]
assoc n f = go
  where go []         = pure []
        go (ab@(a,b):as)
          | a == n    = ((:).(,) a) <$> f b <*> go as
          | otherwise = (ab:) <$> go as

assoc2 :: (Eq b, Functor f) => b -> ([a] -> f [a]) -> [(b, a)] -> f [(b, a)]
assoc2 k f = fmap (uncurry ((++) . fmap ((,) k))) .
             _1 (f . fmap snd) . partition ((==k) . fst)
