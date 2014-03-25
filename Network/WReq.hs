{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, OverloadedStrings,
    TemplateHaskell, GADTs #-}

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
    , Options
    , manager
    , proxy
    , auth
    , Response
    , respHeaders
    , respBody
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
import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable, Data)
import Data.IORef (IORef, newIORef)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (BodyReader, Manager, ManagerSettings, requestBody)
import Network.HTTP.Client.Internal (Proxy(..), addProxy)
import Network.HTTP.Types (Header, HeaderName)
import Prelude hiding (head)
import System.IO (Handle)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP

data Response a = Response {
    _respHeaders :: [Header]
  , _respBody :: a
  } deriving (Functor, Eq, Read, Show, Typeable)
makeLenses ''Response

data Options = Options {
    _manager :: Either ManagerSettings Manager
  , _proxy :: Maybe Proxy
  , _auth :: Maybe (S.ByteString, S.ByteString)
  , _headers :: [Header]
  } deriving (Typeable)
makeLenses ''Options

defaults :: Options
defaults = Options {
    _manager = Left TLS.tlsManagerSettings
  , _proxy   = Nothing
  , _auth    = Nothing
  , _headers = []
  }

get :: String -> IO (Response L.ByteString)
get url = getWith defaults url

getWith :: Options -> String -> IO (Response L.ByteString)
getWith opts url = request id opts url readResponse

type ContentType = S.ByteString

data Payload where
    NoPayload :: Payload
    Raw       :: ContentType -> S.ByteString -> Payload
    Params    :: [(S.ByteString, S.ByteString)] -> Payload
    JSON      :: ToJSON a => a -> Payload
  deriving (Typeable)

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
  return Response {
      _respHeaders = HTTP.responseHeaders resp
    , _respBody = L.fromChunks chunks
    }

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
request modify opts url body = case _manager opts of
                          Left settings -> HTTP.withManager settings go
                          Right manager -> go manager
  where
    go mgr = do
      req <- (modify . setAuth opts . setProxy opts) <$> HTTP.parseUrl url
      HTTP.withResponse req mgr body

setAuth :: Options -> HTTP.Request -> HTTP.Request
setAuth opts req = case _auth opts of
                     Nothing   -> req
                     Just cred -> uncurry HTTP.applyBasicAuth cred req

setProxy :: Options -> HTTP.Request -> HTTP.Request
setProxy opts req = case _proxy opts of
                      Nothing                -> req
                      Just (Proxy host port) -> addProxy host port req

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

json :: FromJSON a => Response L.ByteString -> IO (Response a)
json resp = do
  let contentType = fromMaybe "unknown" . lookup "Content-Type" .
                    _respHeaders $ resp
  unless (contentType == "application/json") $
    throwIO (JSONError $ "content type of response is " ++ show contentType)
  case Aeson.eitherDecode' (_respBody resp) of
    Left err  -> throwIO (JSONError err)
    Right val -> return (fmap (const val) resp)

getHeader :: HeaderName -> Response a -> [S.ByteString]
getHeader name = map snd . filter ((== name) . fst) . _respHeaders

main = do
  resp <- get "http://x.org/"
  return (resp^.respBody)
