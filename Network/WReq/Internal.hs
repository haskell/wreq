{-# LANGUAGE CPP, OverloadedStrings #-}

module Network.WReq.Internal
    (
      defaults
    , emptyMethodWith
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
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Lens.Family ((.~), (%~))
import Network.HTTP.Client (BodyReader)
import Network.HTTP.Client.Internal (Proxy(..), Request, Response(..), addProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName)
import Network.WReq.Types (Auth(..), Options(..), Payload(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Internal.Lens as Int
import qualified Network.WReq.Lens as Lens

-- This mess allows this module to continue to load during interactive
-- development in ghci :-(
#if defined(VERSION_base)
import Paths_wreq (version)
#else
import Data.Version (Version(..))
version = Version [0] ["wip"]
#endif

defaults :: Options
defaults = Options {
    manager   = Left tlsManagerSettings
  , proxy     = Nothing
  , auth      = Nothing
  , headers   = [("User-Agent", userAgent)]
  , params    = []
  , redirects = 10
  }
  where userAgent = "haskell wreq-" <> Char8.pack (showVersion version)

setPayload :: Payload -> Request -> Request
setPayload payload =
  case payload of
    NoPayload -> id
    Raw ct bs -> setHeader "Content-Type" ct .
                 (Int.requestBody .~ HTTP.RequestBodyBS bs)
    Params ps -> HTTP.urlEncodedBody ps
    JSON val  -> setHeader "Content-Type" "application/json" .
                 (Int.requestBody .~ HTTP.RequestBodyLBS (Aeson.encode val))

setRedirects :: Options -> Request -> Request
setRedirects opts req
  | redirects opts == HTTP.redirectCount req = req
  | otherwise = req { HTTP.redirectCount = redirects opts }

setHeader :: HeaderName -> S.ByteString -> Request -> Request
setHeader name value =
  Int.requestHeaders %~ ((name,value) :)

emptyMethodWith :: HTTP.Method -> Options -> String -> IO (Response ())
emptyMethodWith method opts url =
  request (Int.method .~ method) opts url ignoreResponse

ignoreResponse :: Response BodyReader -> IO (Response ())
ignoreResponse resp = (Lens.responseBody .~ ()) <$> readResponse resp

readResponse :: Response BodyReader -> IO (Response L.ByteString)
readResponse resp = do
  chunks <- HTTP.brConsume (HTTP.responseBody resp)
  return resp { responseBody = L.fromChunks chunks }

foldResponseBody :: (a -> S.ByteString -> IO a) -> a
                 -> Response BodyReader -> IO a
foldResponseBody f z0 resp = go z0
  where go z = do
          bs <- HTTP.brRead (HTTP.responseBody resp)
          if S.null bs
            then return z
            else f z bs >>= go

request :: (Request -> Request) -> Options -> String
        -> (Response BodyReader -> IO a) -> IO a
request modify opts url body =
    either (flip HTTP.withManager go) go (manager opts)
  where
    go mgr = do
      let mods = setHeaders . setQuery opts . setAuth opts . setProxy opts .
                 setRedirects opts
      req <- (modify . mods) <$> HTTP.parseUrl url
      HTTP.withResponse req mgr body
    setHeaders = Int.requestHeaders %~ (headers opts ++)

setQuery :: Options -> Request -> Request
setQuery opts =
  case params opts of
    [] -> id
    ps -> Int.queryString %~ \qs ->
          let n = S.length qs in
          qs <> (if n > 1 then "&" else "") <> HTTP.renderSimpleQuery (n==0) ps

setAuth :: Options -> Request -> Request
setAuth = maybe id f . auth
  where
    f (BasicAuth user pass) = HTTP.applyBasicAuth user pass
    f (OAuth2Bearer token)  = setHeader "Authorization" ("Bearer " <> token)
    f (OAuth2Token token)   = setHeader "Authorization" ("token " <> token)

setProxy :: Options -> Request -> Request
setProxy = maybe id f . proxy
  where f (Proxy host port) = addProxy host port
