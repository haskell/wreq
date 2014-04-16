{-# LANGUAGE CPP, GADTs, OverloadedStrings #-}

module Network.WReq.Internal
    (
      defaults
    , defaultManagerSettings
    , emptyMethodWith
    , foldResponseBody
    , ignoreResponse
    , readResponse
    , request
    , requestIO
    , setPayload
    ) where

import Control.Applicative ((<$>))
import Control.Lens ((&), (.~), (%~))
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Network.HTTP.Client (BodyReader)
import Network.HTTP.Client.Internal (Proxy(..), Request, Response(..), addProxy)
import Network.HTTP.Client.MultipartFormData (formDataBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.WReq.Types (Auth(..), Options(..), Payload(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Lens as Lens
import qualified Network.WReq.Lens.Internal as Int
import Network.WReq.Lens.Internal (setHeader)

-- This mess allows this module to continue to load during interactive
-- development in ghci :-(
#if defined(VERSION_base)
import Paths_wreq (version)
#else
import Data.Version (Version(..))
version = Version [0] ["wip"]
#endif

defaultManagerSettings :: HTTP.ManagerSettings
defaultManagerSettings = tlsManagerSettings

defaults :: Options
defaults = Options {
    manager   = Left defaultManagerSettings
  , proxy     = Nothing
  , auth      = Nothing
  , headers   = [("User-Agent", userAgent)]
  , params    = []
  , redirects = 10
  , cookies   = HTTP.createCookieJar []
  }
  where userAgent = "haskell wreq-" <> Char8.pack (showVersion version)

setPayload :: Payload a -> Request -> IO Request
setPayload payload req =
  case payload of
    Raw ct bs   -> return $ req & setHeader "Content-Type" ct &
                            Int.requestBody .~ HTTP.RequestBodyBS bs
    Params ps   -> return $ HTTP.urlEncodedBody ps req
    JSON val    -> return $
                   req & setHeader "Content-Type" "application/json" &
                   Int.requestBody .~ HTTP.RequestBodyLBS (Aeson.encode val)
    FormData ps -> formDataBody ps req

setRedirects :: Options -> Request -> Request
setRedirects opts req
  | redirects opts == HTTP.redirectCount req = req
  | otherwise = req { HTTP.redirectCount = redirects opts }

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

requestIO :: (Request -> IO Request) -> Options -> String
          -> (Response BodyReader -> IO a) -> IO a
requestIO modify opts url body =
    either (flip HTTP.withManager go) go (manager opts)
  where
    go mgr = do
      let frob req = req
                   & Int.requestHeaders %~ (headers opts ++)
                   & setQuery opts
                   & setAuth opts
                   & setProxy opts
                   & setRedirects opts
                   & Int.cookieJar .~ Just (cookies opts)
      req <- modify =<< (frob <$> HTTP.parseUrl url)
      HTTP.withResponse req mgr body

request :: (Request -> Request) -> Options -> String
        -> (Response BodyReader -> IO a) -> IO a
request f = requestIO (return . f)

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
