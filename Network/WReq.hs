{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GADTs, OverloadedStrings #-}

module Network.WReq
    (
      get
    , Payload(..)
    , post
    , binary
    , json
    , JSONError(..)
    , head
    , options
    , put
    , delete
    , Response
    , Lens.responseStatus
    , Lens.responseVersion
    , Lens.responseHeaders
    , Lens.responseBody
    , Lens.responseCookieJar
    , Options
    , Lens.manager
    , Lens.proxy
    , Lens.auth
    , Lens.headers
    , Lens.params
    , Proxy(Proxy)
    , Lens.proxyHost
    , Lens.proxyPort
    , getWith
    , postWith
    , headWith
    , optionsWith
    , putWith
    , deleteWith
    , foldGet
    , foldGetWith
    ) where

import Control.Applicative ((<$>))
import Lens.Family
import Control.Failure (Failure(failure))
import Control.Monad (unless)
import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (BodyReader, requestBody)
import Network.HTTP.Client.Internal (Proxy(..), Response(..), addProxy)
import Network.HTTP.Types (HeaderName)
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Lens as Lens
import qualified Network.WReq.Internal.Lens as Int
import Network.WReq.Types (JSONError(..), Options(..), Payload(..))

defaults :: Options
defaults = Options {
    manager = Left TLS.tlsManagerSettings
  , proxy   = Nothing
  , auth    = Nothing
  , headers = []
  , params  = []
  }

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
  request (setPayload payload . setMethod HTTP.methodPost) opts url readResponse

setPayload :: Payload -> HTTP.Request -> HTTP.Request
setPayload payload =
  case payload of
    NoPayload -> id
    Raw ct bs -> setHeader "Content-Type" ct .
                 setBody (HTTP.RequestBodyBS bs)
    Params ps -> HTTP.urlEncodedBody ps
    JSON val  -> setHeader "Content-Type" "application/json" .
                 setBody (HTTP.RequestBodyLBS (Aeson.encode val))

setHeader :: HeaderName -> S.ByteString -> HTTP.Request -> HTTP.Request
setHeader name value req = req {
    HTTP.requestHeaders = (name, value) : newHeaders
  } where newHeaders = filter ((/= name) . fst) (HTTP.requestHeaders req)

setBody :: HTTP.RequestBody -> HTTP.Request -> HTTP.Request
setBody body req = req { requestBody = body }

setMethod :: HTTP.Method -> HTTP.Request -> HTTP.Request
setMethod method req = req { HTTP.method = method }

head :: String -> IO (Response ())
head = headWith defaults

headWith :: Options -> String -> IO (Response ())
headWith = emptyMethodWith HTTP.methodHead

put :: String -> Payload -> IO (Response ())
put = putWith defaults

putWith :: Options -> String -> Payload -> IO (Response ())
putWith opts url payload =
  request (setPayload payload . setMethod HTTP.methodPost) opts url
  ignoreResponse

options :: String -> IO (Response ())
options = optionsWith defaults

optionsWith :: Options -> String -> IO (Response ())
optionsWith = emptyMethodWith HTTP.methodOptions

delete :: String -> IO (Response ())
delete = deleteWith defaults

deleteWith :: Options -> String -> IO (Response ())
deleteWith = emptyMethodWith HTTP.methodDelete

emptyMethodWith :: HTTP.Method -> Options -> String -> IO (Response ())
emptyMethodWith method opts url =
  request (setMethod method) opts url ignoreResponse

ignoreResponse :: HTTP.Response BodyReader -> IO (Response ())
ignoreResponse resp = fmap (const ()) <$> readResponse resp

readResponse :: HTTP.Response BodyReader -> IO (Response L.ByteString)
readResponse resp = do
  chunks <- HTTP.brConsume (HTTP.responseBody resp)
  return resp { responseBody = L.fromChunks chunks }

foldGet :: (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGet f z url = foldGetWith defaults f z url

foldGetWith :: Options -> (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGetWith opts f z0 url = request id opts url (foldResponseBody f z0)

foldResponseBody :: (a -> S.ByteString -> IO a) -> a
                 -> HTTP.Response BodyReader -> IO a
foldResponseBody f z0 resp = go z0
  where go z = do
          bs <- HTTP.brRead (HTTP.responseBody resp)
          if S.null bs
            then return z
            else f z bs >>= go

request :: (HTTP.Request -> HTTP.Request)
        -> Options -> String -> (HTTP.Response BodyReader -> IO a) -> IO a
request modify opts url body = case manager opts of
                          Left settings -> HTTP.withManager settings go
                          Right mgr     -> go mgr
  where
    go mgr = do
      let mods = setQuery opts . setAuth opts . setProxy opts
      req <- (modify . mods) <$> HTTP.parseUrl url
      HTTP.withResponse req mgr body

setQuery :: Options -> HTTP.Request -> HTTP.Request
setQuery opts req =
  case params opts of
    [] -> req
    ps -> req { HTTP.queryString = HTTP.renderSimpleQuery True ps }

setAuth :: Options -> HTTP.Request -> HTTP.Request
setAuth opts req = case auth opts of
                     Nothing   -> req
                     Just cred -> uncurry HTTP.applyBasicAuth cred req

setProxy :: Options -> HTTP.Request -> HTTP.Request
setProxy opts req = case proxy opts of
                      Nothing                -> req
                      Just (Proxy host port) -> addProxy host port req

json :: (Failure JSONError m, FromJSON a) =>
        Response L.ByteString -> m (Response a)
{-# SPECIALIZE json :: (FromJSON a) =>
                       Response L.ByteString -> IO (Response a) #-}
json resp = do
  let contentType = fromMaybe "unknown" . lookup "Content-Type" .
                    responseHeaders $ resp
  unless (contentType == "application/json") $
    failure (JSONError $ "content type of response is " ++ show contentType)
  case Aeson.eitherDecode' (responseBody resp) of
    Left err  -> failure (JSONError err)
    Right val -> return (fmap (const val) resp)
