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

import Lens.Family.TH (mkLensesBy)
import qualified Network.HTTP.Client as HTTP
import qualified Network.WReq.Types as Types
import qualified Network.HTTP.Types.Status as HTTP
import Control.Lens
import Data.ByteString
import Data.CaseInsensitive

mkLensesBy Just ''Types.Options
mkLensesBy Just ''HTTP.Proxy
mkLensesBy Just ''HTTP.Response
mkLensesBy Just ''HTTP.Status

responseHeader :: CI ByteString -> Traversal' (HTTP.Response body) ByteString
responseHeader x = responseHeaders . traverse . itraversed . indices (==x)
