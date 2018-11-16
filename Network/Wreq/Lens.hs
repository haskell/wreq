{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.Wreq.Lens
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client lens machinery.
--
-- When reading the examples in this module, you should assume the
-- following environment:
--
-- @
-- \-\- Make it easy to write literal 'S.ByteString' and 'Text' values.
-- \{\-\# LANGUAGE OverloadedStrings \#\-\}
--
-- \-\- Our handy module.
-- import "Network.Wreq"
--
-- \-\- Operators such as ('&') and ('.~').
-- import "Control.Lens"
--
-- \-\- Conversion of Haskell values to JSON.
-- import "Data.Aeson" ('Data.Aeson.toJSON')
--
-- \-\- Easy traversal of JSON data.
-- import "Data.Aeson.Lens" ('Data.Aeson.Lens.key', 'Data.Aeson.Lens.nth')
-- @

module Network.Wreq.Lens
    (
    -- * Configuration
      Options
    , manager
    , proxy
    , auth
    , header
    , param
    , redirects
    , headers
    , params
    , cookie
    , cookies
    , ResponseChecker
    , checkResponse

    -- ** Proxy setup
    , Proxy
    , proxyHost
    , proxyPort

    -- * Cookie
    , Cookie
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

    -- * Response
    , Response
    , responseBody
    , responseHeader
    , responseLink
    , responseCookie
    , responseHeaders
    , responseCookieJar
    , responseStatus
    , responseVersion

    -- * HistoriedResponse
    , HistoriedResponse
    , hrFinalResponse
    , hrFinalRequest
    , hrRedirects

    -- ** Status
    , Status
    , statusCode
    , statusMessage

    -- * Link header
    , Link
    , linkURL
    , linkParams

    -- * POST body part
    , Part
    , partName
    , partFileName
    , partContentType
    , partGetBody

    -- * Parsing
    , atto
    , atto_
    ) where

import Control.Applicative ((<*))
import Control.Lens (Fold, Lens, Lens', Traversal', folding)
import Data.Attoparsec.ByteString (Parser, endOfInput, parseOnly)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (Cookie, CookieJar, Request, Manager, ManagerSettings, Proxy, HistoriedResponse)
import Network.HTTP.Client (RequestBody, Response)
import Network.HTTP.Client.MultipartFormData (Part)
import Network.HTTP.Types.Header (Header, HeaderName, ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Version (HttpVersion)
import Network.Mime (MimeType)
import Network.Wreq.Types (Auth, Link, Options, ResponseChecker)
import qualified Network.Wreq.Lens.TH as TH

-- | A lens onto configuration of the connection manager provided by
-- the http-client package.
--
-- In this example, we enable the use of OpenSSL for (hopefully)
-- secure connections:
--
-- @
--import "OpenSSL.Session" ('OpenSSL.Session.context')
--import "Network.HTTP.Client.OpenSSL"
--
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'manager' 'Control.Lens..~' Left ('Network.HTTP.Client.OpenSSL.opensslManagerSettings' 'OpenSSL.Session.context')
--'Network.HTTP.Client.OpenSSL.withOpenSSL' $
--  'Network.Wreq.getWith' opts \"https:\/\/httpbin.org\/get\"
-- @
--
-- In this example, we also set the response timeout to 10000 microseconds:
--
-- @
--import "OpenSSL.Session" ('OpenSSL.Session.context')
--import "Network.HTTP.Client.OpenSSL"
--import "Network.HTTP.Client" ('Network.HTTP.Client.defaultManagerSettings', 'Network.HTTP.Client.managerResponseTimeout')
--
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'manager' 'Control.Lens..~' Left ('Network.HTTP.Client.OpenSSL.opensslManagerSettings' 'OpenSSL.Session.context')
--                    'Control.Lens.&' 'manager' 'Control.Lens..~' Left ('Network.HTTP.Client.defaultManagerSettings' { 'Network.HTTP.Client.managerResponseTimeout' = responseTimeoutMicro 10000 } )
--
--'Network.HTTP.Client.OpenSSL.withOpenSSL' $
--  'Network.Wreq.getWith' opts \"https:\/\/httpbin.org\/get\"
-- @
manager :: Lens' Options (Either ManagerSettings Manager)
manager = TH.manager

-- | A lens onto proxy configuration.
--
-- Example:
--
-- @
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'proxy' 'Control.Lens.?~' 'Network.Wreq.httpProxy' \"localhost\" 8000
--'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
-- @
--
-- Note here the use of the 'Control.Lens.?~' setter to turn a 'Proxy'
-- into a 'Maybe' 'Proxy', to make the type of the RHS compatible with
-- the 'Lens.proxy' lens.
proxy :: Lens' Options (Maybe Proxy)
proxy = TH.proxy

-- | A lens onto request authentication.
--
-- Example (note the use of TLS):
--
-- @
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'Lens.auth' 'Control.Lens.?~' 'Network.Wreq.basicAuth' \"user\" \"pass\"
--'Network.Wreq.getWith' opts \"https:\/\/httpbin.org\/basic-auth\/user\/pass\"
-- @
auth :: Lens' Options (Maybe Auth)
auth = TH.auth

-- | A lens onto all headers with the given name (there can
-- legitimately be zero or more).
--
-- Example:
--
-- @
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'header' \"Accept\" 'Control.Lens..~' [\"*\/*\"]
--'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
-- @
header :: HeaderName -> Lens' Options [ByteString]
header = TH.header

-- | A lens onto all headers (there can legitimately be zero or more).
--
-- In this example, we print all the headers sent by default with
-- every request.
--
-- @
--print ('Network.Wreq.defaults' 'Control.Lens.^.' 'headers')
-- @
headers :: Lens' Options [Header]
headers = TH.headers

-- | A lens onto all query parameters with the given name (there can
-- legitimately be zero or more).
--
-- In this example, we construct the query URL
-- \"@http:\/\/httpbin.org\/get?foo=bar&foo=quux@\".
--
-- @
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'param' \"foo\" 'Control.Lens..~' [\"bar\", \"quux\"]
--'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
-- @
param :: Text -> Lens' Options [Text]
param = TH.param

-- | A lens onto all query parameters.
params :: Lens' Options [(Text, Text)]
params = TH.params

-- | A lens onto the maximum number of redirects that will be followed
-- before an exception is thrown.
--
-- In this example, a 'Network.HTTP.Client.HttpException' will be
-- thrown with a 'Network.HTTP.Client.TooManyRedirects' constructor,
-- because the maximum number of redirects allowed will be exceeded.
--
-- @
--let opts = 'Network.Wreq.defaults' 'Control.Lens.&' 'redirects' 'Control.Lens..~' 3
--'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/redirect\/5\"
-- @
redirects :: Lens' Options Int
redirects = TH.redirects

-- | A lens to get the optional status check function
checkResponse :: Lens' Options (Maybe ResponseChecker)
checkResponse = TH.checkResponse

-- | A traversal onto the cookie with the given name, if one exists.
--
-- N.B. This is an \"illegal\" 'Traversal'': we can change the
-- 'cookieName' of the associated 'Cookie' so that it differs from the
-- name provided to this function.
cookie :: ByteString -> Traversal' Options Cookie
cookie = TH.cookie

-- | A lens onto all cookies.
cookies :: Lens' Options (Maybe CookieJar)
cookies = TH.cookies

-- | A lens onto the name of a cookie.
cookieName :: Lens' Cookie ByteString
cookieName = TH.cookieName

-- | A lens onto the value of a cookie.
cookieValue :: Lens' Cookie ByteString
cookieValue = TH.cookieValue

-- | A lens onto the expiry time of a cookie.
cookieExpiryTime :: Lens' Cookie UTCTime
cookieExpiryTime = TH.cookieExpiryTime

-- | A lens onto the domain of a cookie.
cookieDomain :: Lens' Cookie ByteString
cookieDomain = TH.cookieDomain

-- | A lens onto the path of a cookie.
cookiePath :: Lens' Cookie ByteString
cookiePath = TH.cookiePath

-- | A lens onto the creation time of a cookie.
cookieCreationTime :: Lens' Cookie UTCTime
cookieCreationTime = TH.cookieCreationTime

-- | A lens onto the last access time of a cookie.
cookieLastAccessTime :: Lens' Cookie UTCTime
cookieLastAccessTime = TH.cookieLastAccessTime

-- | A lens onto whether a cookie is persistent across sessions (also
-- known as a \"tracking cookie\").
cookiePersistent :: Lens' Cookie Bool
cookiePersistent = TH.cookiePersistent

-- | A lens onto whether a cookie is host-only.
cookieHostOnly :: Lens' Cookie Bool
cookieHostOnly = TH.cookieHostOnly

-- | A lens onto whether a cookie is secure-only, such that it will
-- only be used over TLS.
cookieSecureOnly :: Lens' Cookie Bool
cookieSecureOnly = TH.cookieSecureOnly

-- | A lens onto whether a cookie is \"HTTP-only\".
--
-- Such cookies should be used only by browsers when transmitting HTTP
-- requests.  They must be unavailable in non-browser environments,
-- such as when executing JavaScript scripts.
cookieHttpOnly :: Lens' Cookie Bool
cookieHttpOnly = TH.cookieHttpOnly

-- | A lens onto the hostname portion of a proxy configuration.
proxyHost :: Lens' Proxy ByteString
proxyHost = TH.proxyHost

-- | A lens onto the TCP port number of a proxy configuration.
proxyPort :: Lens' Proxy Int
proxyPort = TH.proxyPort

-- | A lens onto the status of an HTTP response.
responseStatus :: Lens' (Response body) Status
responseStatus = TH.responseStatus

-- | A lens onto the version of an HTTP response.
responseVersion :: Lens' (Response body) HttpVersion
responseVersion = TH.responseVersion

-- | A lens onto all matching named headers in an HTTP response.
--
-- To access exactly one header (the result will be the empty string if
-- there is no match), use the ('Control.Lens.^.') operator.
--
-- @
--r <- 'Network.Wreq.get' \"http:\/\/httpbin.org\/get\"
--print (r 'Control.Lens.^.' 'responseHeader' \"Content-Type\")
-- @
--
-- To access at most one header (the result will be 'Nothing' if there
-- is no match), use the ('Control.Lens.^?') operator.
--
-- @
--r <- 'Network.Wreq.get' \"http:\/\/httpbin.org\/get\"
--print (r 'Control.Lens.^?' 'responseHeader' \"Content-Transfer-Encoding\")
-- @
--
-- To access all (zero or more) matching headers, use the
-- ('Control.Lens.^..') operator.
--
-- @
--r <- 'Network.Wreq.get' \"http:\/\/httpbin.org\/get\"
--print (r 'Control.Lens.^..' 'responseHeader' \"Set-Cookie\")
-- @
responseHeader :: HeaderName
               -- ^ Header name to match.
               -> Traversal' (Response body) ByteString
responseHeader = TH.responseHeader

-- | A lens onto all headers in an HTTP response.
responseHeaders :: Lens' (Response body) ResponseHeaders
responseHeaders = TH.responseHeaders

-- | A fold over @Link@ headers, matching on both parameter name
-- and value.
--
-- For example, here is a @Link@ header returned by the GitHub search API.
--
-- > Link:
-- >   <https://api.github.com/search/code?q=addClass+user%3Amozilla&page=2>; rel="next",
-- >   <https://api.github.com/search/code?q=addClass+user%3Amozilla&page=34>; rel="last"
--
-- And here is an example of how we can retrieve the URL for the @next@ link
-- programatically.
--
-- @
--r <- 'Network.Wreq.get' \"https:\/\/api.github.com\/search\/code?q=addClass+user:mozilla\"
--print (r 'Control.Lens.^?' 'responseLink' \"rel\" \"next\" . 'linkURL')
-- @
responseLink :: ByteString
             -- ^ Parameter name to match.
             -> ByteString
             -- ^ Parameter value to match.
             -> Fold (Response body) Link
responseLink = TH.responseLink

-- | A lens onto the body of a response.
--
-- @
--r <- 'Network.Wreq.get' \"http:\/\/httpbin.org\/get\"
--print (r 'Control.Lens.^.' 'responseBody')
-- @
responseBody :: Lens (Response body0) (Response body1) body0 body1
responseBody = TH.responseBody

-- | A fold over any cookies that match the given name.
--
-- @
--r <- 'Network.Wreq.get' \"http:\/\/www.nytimes.com\/\"
--print (r 'Control.Lens.^?' responseCookie \"RMID\")
-- @
responseCookie :: ByteString
               -- ^ Name of cookie to match.
               -> Fold (Response body) Cookie
responseCookie = TH.responseCookie

-- | A lens onto all cookies set in the response.
responseCookieJar :: Lens' (Response body) CookieJar
responseCookieJar = TH.responseCookieJar

-- | A lens onto the final request of a historied response.
hrFinalRequest :: Lens' (HistoriedResponse body) Request
hrFinalRequest = TH.hrFinalRequest

-- | A lens onto the final response of a historied response.
hrFinalResponse :: Lens' (HistoriedResponse body) (Response body)
hrFinalResponse = TH.hrFinalResponse

-- | A lens onto the list of redirects of a historied response.
hrRedirects :: Lens' (HistoriedResponse body) [(Request, Response L.ByteString)]
hrRedirects = TH.hrRedirects

-- | A lens onto the numeric identifier of an HTTP status.
statusCode :: Lens' Status Int
statusCode = TH.statusCode

-- | A lens onto the textual description of an HTTP status.
statusMessage :: Lens' Status ByteString
statusMessage = TH.statusMessage

-- | A lens onto the URL portion of a @Link@ element.
linkURL :: Lens' Link ByteString
linkURL = TH.linkURL

-- | A lens onto the parameters of a @Link@ element.
linkParams :: Lens' Link [(ByteString, ByteString)]
linkParams = TH.linkParams

-- | A lens onto the name of the @<input>@ element associated with
-- part of a multipart form upload.
partName :: Lens' Part Text
partName = TH.partName

-- | A lens onto the filename associated with part of a multipart form
-- upload.
partFileName :: Lens' Part (Maybe String)
partFileName = TH.partFilename

-- | A lens onto the content-type associated with part of a multipart
-- form upload.
partContentType :: Traversal' Part (Maybe MimeType)
partContentType = TH.partContentType

-- | A lens onto the code that fetches the data associated with part
-- of a multipart form upload.
partGetBody :: Lens' Part (IO RequestBody)
partGetBody = TH.partGetBody

-- | Turn an attoparsec 'Parser' into a 'Fold'.
--
-- Both headers and bodies can contain complicated data that we may
-- need to parse.
--
-- Example: when responding to an OPTIONS request, a server may return
-- the list of verbs it supports in any order, up to and including
-- changing the order on every request (which httpbin.org /actually
-- does/!).  To deal with this possibility, we parse the list, then
-- sort it.
--
-- >>> import Data.Attoparsec.ByteString.Char8 as A
-- >>> import Data.List (sort)
-- >>>
-- >>> let comma = skipSpace >> "," >> skipSpace
-- >>> let verbs = A.takeWhile isAlpha_ascii `sepBy` comma
-- >>>
-- >>> r <- options "http://httpbin.org/get"
-- >>> r ^. responseHeader "Allow" . atto verbs . to sort
-- ["GET","HEAD","OPTIONS"]
atto :: Parser a -> Fold ByteString a
atto = folding . parseOnly

-- | The same as 'atto', but ensures that the parser consumes the
-- entire input.
--
-- Equivalent to:
--
-- @
--'atto_' myParser = 'atto' (myParser '<*' 'endOfInput')
-- @
atto_ :: Parser a -> Fold ByteString a
atto_ p = atto (p <* endOfInput)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Aeson (toJSON)
-- >>> import Data.Aeson.Lens (key, nth)
-- >>> import Network.Wreq
