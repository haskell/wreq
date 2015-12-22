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
    , checkStatus

    , H.Cookie
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

    , H.Proxy
    , proxyHost
    , proxyPort

    , H.Response
    , responseStatus
    , responseVersion
    , responseHeader
    , responseHeaders
    , responseLink
    , responseBody
    , responseCookie
    , responseCookieJar
    , responseClose'

    , H.Status
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
    , partHeaders
    ) where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Lens.Micro
import           Lens.Micro.TH hiding (makeLenses)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as H
import           Network.Wreq.Internal.Lens (assoc, assoc2)
import           Network.Wreq.Internal.Link
import           Network.Wreq.Lens.Extra (folding)
import           Network.Wreq.Lens.Machinery
import qualified Network.Wreq.Types as Types

---

makeLenses ''Types.Options
makeLensesWith (lensRules & lensField .~ fieldName toCamelCase) ''H.Cookie
makeLenses ''H.Proxy
makeLenses ''H.Response
makeLenses ''H.Status
makeLenses ''Types.Link
makeLenses ''Form.Part

responseHeader :: H.HeaderName -> Traversal' (H.Response body) ByteString
responseHeader n = responseHeaders . assoc n

param :: Text -> Lens' Types.Options [Text]
param n = params . assoc2 n

header :: H.HeaderName -> Lens' Types.Options [ByteString]
header n = headers . assoc2 n

-- Colin: This has been specialized to a Lens' from an Iso'. Is this good
-- enough?
_CookieJar :: Lens' H.CookieJar [H.Cookie]
_CookieJar = lens H.destroyCookieJar (const H.createCookieJar)

-- N.B. This is an "illegal" traversal because we can change its cookie_name.
cookie :: ByteString -> Traversal' Types.Options H.Cookie
cookie name = cookies . _Just . _CookieJar . traverse . filtered
              (\c -> H.cookie_name c == name)

responseCookie :: ByteString -> Fold (H.Response body) H.Cookie
responseCookie name =
  responseCookieJar . folding H.destroyCookieJar . filtered
  ((==name) . H.cookie_name)

responseLink :: ByteString -> ByteString -> Fold (H.Response body) Types.Link
responseLink name val =
  responseHeader "Link" . folding links .
  filtered (has (linkParams . folded . filtered (== (name,val))))
