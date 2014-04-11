{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes,
    TemplateHaskell #-}
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
    , redirects
    , cookies

    , HTTP.Cookie
    , cookie_name
    , cookie_value
    , cookie_expiry_time
    , cookie_domain
    , cookie_path
    , cookie_creation_time
    , cookie_last_access_time
    , cookie_persistent
    , cookie_host_only
    , cookie_secure_only
    , cookie_http_only

    , HTTP.Proxy
    , proxyHost
    , proxyPort

    , HTTP.Response
    , responseStatus
    , responseVersion
    , responseHeader
    , responseHeaders
    , responseLink
    , responseBody
    , responseCookie
    , responseCookieJar
    , responseClose'

    , HTTP.Status
    , statusCode
    , statusMessage

    , Types.Link
    , linkURL
    , linkParams
    ) where

import Control.Lens hiding (makeLenses)
import Data.ByteString (ByteString)
import Network.WReq.Lens.Internal (assoc, assoc2)
import Network.WReq.Lens.Machinery (makeLenses)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.WReq.Types as Types
import Network.WReq.Internal.Link

makeLenses ''Types.Options
makeLenses ''HTTP.Cookie
makeLenses ''HTTP.Proxy
makeLenses ''HTTP.Response
makeLenses ''HTTP.Status
makeLenses ''Types.Link

responseHeader :: HTTP.HeaderName -> Traversal' (HTTP.Response body) ByteString
responseHeader n = responseHeaders . assoc n

param :: ByteString -> Lens' Types.Options [ByteString]
param n = params . assoc2 n

header :: HTTP.HeaderName -> Lens' Types.Options [ByteString]
header n = headers . assoc2 n

responseCookie :: ByteString -> Fold (HTTP.Response body) HTTP.Cookie
responseCookie name =
  responseCookieJar . folding HTTP.destroyCookieJar . filtered
  ((==name) . HTTP.cookie_name)

responseLink :: ByteString -> ByteString -> Fold (HTTP.Response body) Types.Link
responseLink name val =
  responseHeader "Link" . folding links .
  filtered (has (linkParams . folded . filtered (== (name,val))))
