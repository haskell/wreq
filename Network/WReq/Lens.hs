{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.WReq.Lens
    (
      Types.Options
    , manager
    , proxy
    , auth
    , headers
    , params
    , HTTP.Proxy
    , proxyHost
    , proxyPort
    , HTTP.Response
    , responseStatus
    , responseVersion
    , responseHeader
    , responseHeaders
    , responseBody
    , responseCookieJar
    , responseClose'
    , HTTP.Status
    , statusCode
    , statusMessage
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.ByteString (ByteString)
import Lens.Family.TH (mkLensesBy)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.WReq.Types as Types

mkLensesBy Just ''Types.Options
mkLensesBy Just ''HTTP.Proxy
mkLensesBy Just ''HTTP.Response
mkLensesBy Just ''HTTP.Status

responseHeader :: Applicative f =>
                  HTTP.HeaderName -> (ByteString -> f ByteString)
               -> HTTP.Response body -> f (HTTP.Response body)
responseHeader n = responseHeaders . assoc n

assoc :: (Eq k, Applicative f) => k -> (a -> f a) -> [(k, a)] -> f [(k, a)]
assoc n f = go
  where go []         = pure []
        go (ab@(a,b):as)
          | a == n    = ((:).(,) a) <$> f b <*> go as
          | otherwise = (ab:) <$> go as
