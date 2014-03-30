{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.WReq.Lens
    (
      Types.Options
    , manager
    , proxy
    , auth
    , header
    , headers
    , param
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

import Control.Applicative (Applicative)
import Data.ByteString (ByteString)
import Lens.Family.TH (mkLensesBy)
import Network.WReq.Internal.Lens (assoc, assoc2)
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

param :: Functor f =>
         ByteString -> ([ByteString] -> f [ByteString]) -> Types.Options
      -> f Types.Options
param n = params . assoc2 n

header :: Functor f =>
          HTTP.HeaderName -> ([ByteString] -> f [ByteString]) -> Types.Options
       -> f Types.Options
header n = headers . assoc2 n
