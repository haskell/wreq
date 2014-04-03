{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

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

    -- * Responses
    , Response
    , Lens.responseStatus
    , Lens.responseVersion
    , Lens.responseHeader
    , Lens.responseHeaders
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
import Network.WReq.Types (Auth(..), JSONError(..), Options(..), Payload(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Lens as Lens
import qualified Network.WReq.Lens.Internal as Int

get :: String -> IO (Response L.ByteString)
get url = getWith defaults url

getWith :: Options -> String -> IO (Response L.ByteString)
getWith opts url = request id opts url readResponse

binary :: S.ByteString -> Payload
binary = Raw "application/octet-stream"

post :: String -> Payload -> IO (Response L.ByteString)
post url payload = postWith defaults url payload

postWith :: Options -> String -> Payload -> IO (Response L.ByteString)
postWith opts url payload =
  request (setPayload payload . (Int.method .~ HTTP.methodPost)) opts url
    readResponse

head :: String -> IO (Response ())
head = headWith (defaults & Lens.redirects .~ 0)

headWith :: Options -> String -> IO (Response ())
headWith = emptyMethodWith HTTP.methodHead

put :: String -> Payload -> IO (Response L.ByteString)
put = putWith defaults

putWith :: Options -> String -> Payload -> IO (Response L.ByteString)
putWith opts url payload =
  request (setPayload payload . (Int.method .~ HTTP.methodPut)) opts url
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
jsonValue = json

basicAuth :: S.ByteString -> S.ByteString -> Maybe Auth
basicAuth user pass = Just (BasicAuth user pass)

oauth2Bearer :: S.ByteString -> Maybe Auth
oauth2Bearer token = Just (OAuth2Bearer token)

oauth2Token :: S.ByteString -> Maybe Auth
oauth2Token token = Just (OAuth2Token token)
