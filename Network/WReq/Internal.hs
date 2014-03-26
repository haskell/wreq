{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GADTs, OverloadedStrings #-}

module Network.WReq.Internal
    (
      emptyMethodWith
    , foldResponseBody
    , ignoreResponse
    , readResponse
    , request
    , setAuth
    , setHeader
    , setPayload
    , setProxy
    , setQuery
    ) where

import Control.Applicative ((<$>))
import Lens.Family ((.~), (%~))
import Network.HTTP.Client (BodyReader)
import Network.HTTP.Client.Internal (Proxy(..), Response(..), addProxy)
import Network.HTTP.Types (HeaderName)
import Network.WReq.Types (Options(..), Payload(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Internal.Lens as Int
import qualified Network.WReq.Lens as Lens


setPayload :: Payload -> HTTP.Request -> HTTP.Request
setPayload payload =
  case payload of
    NoPayload -> id
    Raw ct bs -> setHeader "Content-Type" ct .
                 (Int.requestBody .~ HTTP.RequestBodyBS bs)
    Params ps -> HTTP.urlEncodedBody ps
    JSON val  -> setHeader "Content-Type" "application/json" .
                 (Int.requestBody .~ HTTP.RequestBodyLBS (Aeson.encode val))

setHeader :: HeaderName -> S.ByteString -> HTTP.Request -> HTTP.Request
setHeader name value =
  Int.requestHeaders %~ ((name,value) :) . filter ((/= name) . fst)

emptyMethodWith :: HTTP.Method -> Options -> String -> IO (Response ())
emptyMethodWith method opts url =
  request (Int.method .~ method) opts url ignoreResponse

ignoreResponse :: HTTP.Response BodyReader -> IO (Response ())
ignoreResponse resp = (Lens.responseBody .~ ()) <$> readResponse resp

readResponse :: HTTP.Response BodyReader -> IO (Response L.ByteString)
readResponse resp = do
  chunks <- HTTP.brConsume (HTTP.responseBody resp)
  return resp { responseBody = L.fromChunks chunks }

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
request modify opts url body =
    either (flip HTTP.withManager go) go (manager opts)
  where
    go mgr = do
      let mods = setQuery opts . setAuth opts . setProxy opts
      req <- (modify . mods) <$> HTTP.parseUrl url
      HTTP.withResponse req mgr body

setQuery :: Options -> HTTP.Request -> HTTP.Request
setQuery opts =
  case params opts of
    [] -> id
    ps -> Int.queryString .~ HTTP.renderSimpleQuery True ps

setAuth :: Options -> HTTP.Request -> HTTP.Request
setAuth = maybe id (uncurry HTTP.applyBasicAuth) . auth

setProxy :: Options -> HTTP.Request -> HTTP.Request
setProxy = maybe id f . proxy
  where f (Proxy host port) = addProxy host port
