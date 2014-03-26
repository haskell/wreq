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
ignoreResponse resp = fmap (const ()) <$> readResponse resp

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
