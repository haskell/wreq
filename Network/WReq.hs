{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}

module Network.WReq
    (
      get
    , json
    , JSONError(..)
    , Options
    , manager
    , proxy
    , auth
    , Response
    , respHeaders
    , respBody
    , getWith
    , foldGet
    , foldGetWith
    ) where

import Control.Exception (Exception, throwIO)
import Control.Lens
import Data.Aeson (FromJSON)
import Data.Data (Typeable)
import Data.IORef (IORef, newIORef)
import Network.HTTP.Client (BodyReader, Manager, ManagerSettings)
import Network.HTTP.Client.Internal (Proxy(..), addProxy)
import Network.HTTP.Types (Header)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS

data Response a = Response {
    _respHeaders :: [Header]
  , _respBody :: a
  } deriving (Functor, Eq, Read, Show, Typeable)
makeLenses ''Response

data Options = Options {
    _manager :: Either ManagerSettings Manager
  , _proxy :: Maybe Proxy
  , _auth :: Maybe (S.ByteString, S.ByteString)
  } deriving (Typeable)
makeLenses ''Options

defaults :: Options
defaults = Options {
    _manager = Left TLS.tlsManagerSettings
  , _proxy   = Nothing
  , _auth    = Nothing
  }

get :: String -> IO (Response L.ByteString)
get url = getWith defaults url

getWith :: Options -> String -> IO (Response L.ByteString)
getWith opts url = prepare opts url readResponse

readResponse :: HTTP.Response BodyReader -> IO (Response L.ByteString)
readResponse resp = do
  chunks <- HTTP.brConsume (HTTP.responseBody resp)
  return Response {
      _respHeaders = HTTP.responseHeaders resp
    , _respBody = L.fromChunks chunks
    }

foldGet :: (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGet f z url = foldGetWith defaults f z url

foldGetWith :: Options -> (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGetWith opts f z0 url = prepare opts url (foldResponseBody f z0)

foldResponseBody :: (a -> S.ByteString -> IO a) -> a
                 -> HTTP.Response BodyReader -> IO a
foldResponseBody f z0 resp = go z0
  where go z = do
          bs <- HTTP.brRead (HTTP.responseBody resp)
          if S.null bs
            then return z
            else f z bs >>= go

prepare :: Options -> String -> (HTTP.Response BodyReader -> IO a) -> IO a
prepare opts url body = case _manager opts of
                          Left settings -> HTTP.withManager settings go
                          Right manager -> go manager
  where
    go mgr = do
      req0 <- HTTP.parseUrl url
      let reqAuth = case _auth opts of
                      Nothing   -> req0
                      Just cred -> uncurry HTTP.applyBasicAuth cred req0
          reqProxy = case _proxy opts of
                       Nothing                -> reqAuth
                       Just (Proxy host port) -> addProxy host port reqAuth
      HTTP.withResponse reqProxy mgr body

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

json :: FromJSON a => Response L.ByteString -> IO (Response a)
json resp = case Aeson.eitherDecode' (_respBody resp) of
              Left err  -> throwIO (JSONError err)
              Right val -> return (fmap (const val) resp)

main = do
  resp <- get "http://x.org/"
  return (resp^.respBody)
