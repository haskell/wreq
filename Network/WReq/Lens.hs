{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.WReq.Lens
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client lens machinery.

module Network.WReq.Lens
    (
    -- * Configuration
      Options
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
    , responseStatus
    , responseVersion
    , responseHeader
    , responseHeaders
    , responseLink
    , responseBody
    , responseCookie
    , responseCookieJar

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
    , partFilename
    , partContentType
    , partGetBody
    ) where

import Control.Lens (Fold, Lens, Lens', Traversal')
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (Cookie, CookieJar, Manager, ManagerSettings, Proxy)
import Network.HTTP.Client (RequestBody, Response)
import Network.HTTP.Client.MultipartFormData (Part)
import Network.HTTP.Types.Header (Header, HeaderName, ResponseHeaders)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Version (HttpVersion)
import Network.Mime (MimeType)
import Network.WReq.Types (Auth, Link, Options, Param)
import qualified Network.WReq.Lens.TH as TH

manager :: Lens' Options (Either ManagerSettings Manager)
manager = TH.manager

proxy :: Lens' Options (Maybe Proxy)
proxy = TH.proxy

auth :: Lens' Options (Maybe Auth)
auth = TH.auth

header :: HeaderName -> Lens' Options [ByteString]
header = TH.header

headers :: Lens' Options [Header]
headers = TH.headers

param :: ByteString -> Lens' Options [ByteString]
param = TH.param

params :: Lens' Options [(ByteString, ByteString)]
params = TH.params

redirects :: Lens' Options Int
redirects = TH.redirects

cookie :: ByteString -> Traversal' Options Cookie
cookie = TH.cookie

cookies :: Lens' Options CookieJar
cookies = TH.cookies

cookieName :: Lens' Cookie ByteString
cookieName = TH.cookieName

cookieValue :: Lens' Cookie ByteString
cookieValue = TH.cookieValue

cookieExpiryTime :: Lens' Cookie UTCTime
cookieExpiryTime = TH.cookieExpiryTime

cookieDomain :: Lens' Cookie ByteString
cookieDomain = TH.cookieDomain

cookiePath :: Lens' Cookie ByteString
cookiePath = TH.cookiePath

cookieCreationTime :: Lens' Cookie UTCTime
cookieCreationTime = TH.cookieCreationTime

cookieLastAccessTime :: Lens' Cookie UTCTime
cookieLastAccessTime = TH.cookieLastAccessTime

cookiePersistent :: Lens' Cookie Bool
cookiePersistent = TH.cookiePersistent

cookieHostOnly :: Lens' Cookie Bool
cookieHostOnly = TH.cookieHostOnly

cookieSecureOnly :: Lens' Cookie Bool
cookieSecureOnly = TH.cookieSecureOnly

cookieHttpOnly :: Lens' Cookie Bool
cookieHttpOnly = TH.cookieHttpOnly

proxyHost :: Lens' Proxy ByteString
proxyHost = TH.proxyHost

proxyPort :: Lens' Proxy Int
proxyPort = TH.proxyPort

responseStatus :: Lens' (Response body) Status
responseStatus = TH.responseStatus

responseVersion :: Lens' (Response body) HttpVersion
responseVersion = TH.responseVersion

responseHeader :: HeaderName -> Traversal' (Response body) ByteString
responseHeader = TH.responseHeader

responseHeaders :: Lens' (Response body) ResponseHeaders
responseHeaders = TH.responseHeaders

responseLink :: ByteString -> ByteString -> Fold (Response body) Link
responseLink = TH.responseLink

responseBody :: Lens (Response body0) (Response body1) body0 body1
responseBody = TH.responseBody

responseCookie :: ByteString -> Fold (Response body) Cookie
responseCookie = TH.responseCookie

responseCookieJar :: Lens' (Response body) CookieJar
responseCookieJar = TH.responseCookieJar

statusCode :: Lens' Status Int
statusCode = TH.statusCode

statusMessage :: Lens' Status ByteString
statusMessage = TH.statusMessage

linkURL :: Lens' Link ByteString
linkURL = TH.linkURL

linkParams :: Lens' Link [Param]
linkParams = TH.linkParams

partName :: Lens' Part Text
partName = TH.partName

partFilename :: Lens' Part (Maybe String)
partFilename = TH.partFilename

partContentType :: Lens' Part (Maybe MimeType)
partContentType = TH.partContentType

partGetBody :: Lens' Part (IO RequestBody)
partGetBody = TH.partGetBody
