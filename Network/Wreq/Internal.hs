{-# LANGUAGE CPP, GADTs, OverloadedStrings #-}

module Network.Wreq.Internal
    (
      defaults
    , defaultManagerSettings
    , emptyMethodWith
    , foldResponseBody
    , ignoreResponse
    , readResponse
    , request
    , prepareGet
    , preparePost
    , runRead
    , prepareHead
    , runIgnore
    , prepareOptions
    , preparePut
    , prepareDelete
    , prepareMethod
    ) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Lens ((&), (.~), (%~))
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Version (showVersion)
import Network.HTTP.Client (BodyReader)
import Network.HTTP.Client.Internal (Proxy(..), Request, Response(..), addProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq.Internal.Lens (setHeader)
import Network.Wreq.Internal.Types (Mgr, Req(..), Run)
import Network.Wreq.Types (Auth(..), Options(..), Postable(..), Putable(..))
import Prelude hiding (head)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wreq.Internal.Lens as Lens
import qualified Network.Wreq.Internal.AWS as AWS (signRequest)
import qualified Network.Wreq.Internal.OAuth1 as OAuth1 (signRequest)
import qualified Network.Wreq.Lens as Lens hiding (checkStatus)

-- This mess allows this module to continue to load during interactive
-- development in ghci :-(
#if defined(VERSION_base)
import Paths_wreq (version)
#else
import Data.Version (Version(..))
version :: Version
version = Version [0] ["wip"]
#endif

defaultManagerSettings :: HTTP.ManagerSettings
defaultManagerSettings = tlsManagerSettings

defaults :: Options
defaults = Options {
    manager     = Left defaultManagerSettings
  , proxy       = Nothing
  , auth        = Nothing
  , headers     = [("User-Agent", userAgent)]
  , params      = []
  , redirects   = 10
  , cookies     = Just (HTTP.createCookieJar [])
  , checkStatus = Nothing
  }
  where userAgent = "haskell wreq-" <> Char8.pack (showVersion version)

setRedirects :: Options -> Request -> Request
setRedirects opts req
  | redirects opts == HTTP.redirectCount req = req
  | otherwise = req { HTTP.redirectCount = redirects opts }

emptyMethodWith :: HTTP.Method -> Options -> String -> IO (Response ())
emptyMethodWith method opts url =
  request (return . (Lens.method .~ method)) opts url ignoreResponse

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

request :: (Request -> IO Request) -> Options -> String
        -> (Response BodyReader -> IO a) -> IO a
request modify opts url act = run (manager opts) act =<< prepare modify opts url

run :: Mgr -> (Response BodyReader -> IO a) -> Request -> IO a
run emgr act req = either (flip HTTP.withManager go) go emgr
  where go mgr = HTTP.withResponse req mgr act

prepare :: (Request -> IO Request) -> Options -> String -> IO Request
prepare modify opts url = do
  signRequest =<< modify =<< frob <$> HTTP.parseUrl url
  where
    frob req = req & Lens.requestHeaders %~ (headers opts ++)
                   & setQuery opts
                   & setAuth opts
                   & setProxy opts
                   & setCheckStatus opts
                   & setRedirects opts
                   & Lens.cookieJar .~ cookies opts
    signRequest :: Request -> IO Request
    signRequest = maybe return f $ auth opts
      where
        f (AWSAuth versn key secret) = AWS.signRequest versn key secret
        f (OAuth1 consumerToken consumerSecret token secret) = OAuth1.signRequest consumerToken consumerSecret token secret
        f _ = return


setQuery :: Options -> Request -> Request
setQuery opts =
  case params opts of
    [] -> id
    ps -> Lens.queryString %~ \qs ->
          let n = S.length qs in
          qs <> (if n > 1 then "&" else "") <> HTTP.renderSimpleQuery (n==0)
          (map (encodeUtf8 *** encodeUtf8) ps)

setAuth :: Options -> Request -> Request
setAuth = maybe id f . auth
  where
    f (BasicAuth user pass) = HTTP.applyBasicAuth user pass
    f (OAuth2Bearer token)  = setHeader "Authorization" ("Bearer " <> token)
    f (OAuth2Token token)   = setHeader "Authorization" ("token " <> token)
    -- for AWS request signature, see Internal/AWS
    f (AWSAuth _ _ _)       = id
    f (OAuth1 _ _ _ _)      = id

setProxy :: Options -> Request -> Request
setProxy = maybe id f . proxy
  where f (Proxy host port) = addProxy host port

setCheckStatus :: Options -> Request -> Request
setCheckStatus = maybe id f . checkStatus
  where f cs = ( & Lens.checkStatus .~ cs)

prepareGet :: Options -> String -> IO Req
prepareGet opts url = Req (manager opts) <$> prepare return opts url

runRead :: Run L.ByteString
runRead (Req mgr req) = run mgr readResponse req

preparePost :: Postable a => Options -> String -> a -> IO Req
preparePost opts url payload = Req (manager opts) <$>
  prepare (postPayload payload . (Lens.method .~ HTTP.methodPost)) opts url

prepareMethod :: HTTP.Method -> Options -> String -> IO Req
prepareMethod method opts url = Req (manager opts) <$>
  prepare (return . (Lens.method .~ method)) opts url

prepareHead :: Options -> String -> IO Req
prepareHead = prepareMethod HTTP.methodHead

runIgnore :: Run ()
runIgnore (Req mgr req) = run mgr ignoreResponse req

prepareOptions :: Options -> String -> IO Req
prepareOptions = prepareMethod HTTP.methodOptions

preparePut :: Putable a => Options -> String -> a -> IO Req
preparePut opts url payload = Req (manager opts) <$>
  prepare (putPayload payload . (Lens.method .~ HTTP.methodPut)) opts url

prepareDelete :: Options -> String -> IO Req
prepareDelete = prepareMethod HTTP.methodDelete
