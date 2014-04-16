{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- |
-- Module      : Network.WReq
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- A library for client-side HTTP requests, focused on ease of use.

module Network.WReq
    (
    -- * HTTP verbs
      get
    , post
    , head
    , options
    , put
    , delete
    -- ** Configurable verbs
    , getWith
    , postWith
    , headWith
    , optionsWith
    , putWith
    , deleteWith
    -- ** Incremental consumption of responses
    , foldGet
    , foldGetWith

    -- * Payloads for POST and PUT
    , Payload(..)
    , binary
    -- ** Multipart form data
    , Form.Part
    , Lens.partName
    , Lens.partFilename
    , Lens.partContentType
    , Lens.partGetBody
    -- *** Smart constructors
    , Form.partBS
    , Form.partLBS
    , Form.partFile
    , Form.partFileSource

    -- * Responses
    , Response
    , Lens.responseStatus
    , Lens.responseVersion
    , Lens.responseHeader
    , Lens.responseHeaders
    , Lens.responseLink
    , Lens.responseBody
    , Lens.responseCookie
    , Lens.responseCookieJar
    , Lens.Status
    , Lens.statusCode
    , Lens.statusMessage
    -- ** Decoding responses
    , JSONError(..)
    , json
    , jsonValue

    -- * Cookies
    , Lens.Cookie
    , Lens.cookie_name
    , Lens.cookie_value
    , Lens.cookie_expiry_time
    , Lens.cookie_domain
    , Lens.cookie_path
    , Lens.cookie_creation_time
    , Lens.cookie_last_access_time
    , Lens.cookie_persistent
    , Lens.cookie_host_only
    , Lens.cookie_secure_only
    , Lens.cookie_http_only

    -- * Configuration
    , Options
    , defaults
    , Lens.manager
    , Lens.header
    , Lens.headers
    , Lens.param
    , Lens.params
    , Lens.redirects
    , Lens.cookie
    , Lens.cookies
    -- ** Using a manager with defaults
    , withManager
    -- ** Proxy settings
    , Lens.proxy
    , Proxy(Proxy)
    , Lens.proxyHost
    , Lens.proxyPort
    -- ** Authentication
    , Lens.auth
    , Auth
    , basicAuth
    , oauth2Bearer
    , oauth2Token
    ) where

import Control.Failure (Failure(failure))
import Control.Lens ((.~), (&))
import Control.Monad (unless)
import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client.Internal (Proxy(..), Response(..))
import Network.WReq.Internal
import Network.WReq.Types (Auth(..), JSONError(..), Options(..), Payload(..), SimplePayload(..), Postable(..), Putable(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Lens as Lens
import qualified Network.WReq.Lens.Internal as Int

get :: String -> IO (Response L.ByteString)
get url = getWith defaults url

withManager :: (Options -> IO a) -> IO a
withManager act = HTTP.withManager defaultManagerSettings $ \mgr ->
  act defaults { manager = Right mgr }

getWith :: Options -> String -> IO (Response L.ByteString)
getWith opts url = request id opts url readResponse

binary :: S.ByteString -> SimplePayload
binary = SimpleRaw "application/octet-stream"

post :: Postable a => String -> a -> IO (Response L.ByteString)
post url payload = postWith defaults url payload

postWith :: Postable a => Options -> String -> a -> IO (Response L.ByteString)
postWith opts url payload =
  requestIO (postPayload payload . (Int.method .~ HTTP.methodPost)) opts url
    readResponse

head :: String -> IO (Response ())
head = headWith (defaults & Lens.redirects .~ 0)

headWith :: Options -> String -> IO (Response ())
headWith = emptyMethodWith HTTP.methodHead

put :: Putable a => String -> a -> IO (Response L.ByteString)
put url payload = putWith defaults url payload

putWith :: Putable a => Options -> String -> a -> IO (Response L.ByteString)
putWith opts url payload =
  -- XXX this erroneously sets the method to POST if we try to PUT
  -- multipart form data
  requestIO (putPayload payload . (Int.method .~ HTTP.methodPut)) opts url
    readResponse

options :: String -> IO (Response ())
options = optionsWith defaults

optionsWith :: Options -> String -> IO (Response ())
optionsWith = emptyMethodWith HTTP.methodOptions

delete :: String -> IO (Response ())
delete = deleteWith defaults

deleteWith :: Options -> String -> IO (Response ())
deleteWith = emptyMethodWith HTTP.methodDelete

foldGet :: (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGet f z url = foldGetWith defaults f z url

foldGetWith :: Options -> (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGetWith opts f z0 url = request id opts url (foldResponseBody f z0)

json :: (Failure JSONError m, FromJSON a) =>
        Response L.ByteString -> m (Response a)
{-# SPECIALIZE json :: (FromJSON a) =>
                       Response L.ByteString -> IO (Response a) #-}
json resp = do
  let contentType = fst . S.break (==59) . fromMaybe "unknown" .
                    lookup "Content-Type" . responseHeaders $ resp
  unless ("application/json" `S.isPrefixOf` contentType) $
    failure (JSONError $ "content type of response is " ++ show contentType)
  case Aeson.eitherDecode' (responseBody resp) of
    Left err  -> failure (JSONError err)
    Right val -> return (fmap (const val) resp)

jsonValue :: (Failure JSONError m) =>
             Response L.ByteString -> m (Response Aeson.Value)
{-# SPECIALIZE jsonValue :: Response L.ByteString
                         -> IO (Response Aeson.Value) #-}
jsonValue = json

basicAuth :: S.ByteString -> S.ByteString -> Maybe Auth
basicAuth user pass = Just (BasicAuth user pass)

oauth2Bearer :: S.ByteString -> Maybe Auth
oauth2Bearer token = Just (OAuth2Bearer token)

oauth2Token :: S.ByteString -> Maybe Auth
oauth2Token token = Just (OAuth2Token token)
