{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes,
    TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.Wreq.Lens.TH
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
    , cookie
    , cookies

    , HTTP.Cookie
    , cookieName
    , cookieValue
    , cookieExpiryTime
    , cookieDomain
    , cookiePath
    , cookieCreationTime
    , cookieLastAccessTime
    , cookiePersistent
    , cookieHostOnly
    , cookieSecureOnly
    , cookieHttpOnly

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

    , Form.Part
    , partName
    , partFilename
    , partContentType
    , partGetBody
    ) where

import Control.Lens hiding (makeLenses)
import Control.Lens.TH (defaultRules, lensField, makeLensesWith)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.Wreq.Internal.Lens (assoc, assoc2)
import Network.Wreq.Lens.Machinery (makeLenses, toCamelCase)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wreq.Types as Types
import Network.Wreq.Internal.Link

makeLenses ''Types.Options
makeLensesWith (defaultRules & lensField .~ Just . toCamelCase) ''HTTP.Cookie
makeLenses ''HTTP.Proxy
makeLenses ''HTTP.Response
makeLenses ''HTTP.Status
makeLenses ''Types.Link
makeLenses ''Form.Part

responseHeader :: HTTP.HeaderName -> Traversal' (HTTP.Response body) ByteString
responseHeader n = responseHeaders . assoc n

param :: Text -> Lens' Types.Options [Text]
param n = params . assoc2 n

header :: HTTP.HeaderName -> Lens' Types.Options [ByteString]
header n = headers . assoc2 n

_CookieJar :: Iso' HTTP.CookieJar [HTTP.Cookie]
_CookieJar = iso HTTP.destroyCookieJar HTTP.createCookieJar

-- N.B. This is an "illegal" traversal because we can change its cookie_name.
cookie :: ByteString -> Traversal' Types.Options HTTP.Cookie
cookie name = cookies . _CookieJar . traverse . filtered
              (\c -> HTTP.cookie_name c == name)

responseCookie :: ByteString -> Fold (HTTP.Response body) HTTP.Cookie
responseCookie name =
  responseCookieJar . folding HTTP.destroyCookieJar . filtered
  ((==name) . HTTP.cookie_name)

responseLink :: ByteString -> ByteString -> Fold (HTTP.Response body) Types.Link
responseLink name val =
  responseHeader "Link" . folding links .
  filtered (has (linkParams . folded . filtered (== (name,val))))
