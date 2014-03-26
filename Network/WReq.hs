{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

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

import Control.Failure (Failure(failure))
import Control.Monad (unless)
import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import Lens.Family ((.~))
import Network.HTTP.Client.Internal (Proxy(..), Response(..))
import Network.WReq.Internal
import Network.WReq.Types (JSONError(..), Options(..), Payload(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP
import qualified Network.WReq.Internal.Lens as Int
import qualified Network.WReq.Lens as Lens

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
  request (setPayload payload . (Int.method .~ HTTP.methodPost)) opts url
    readResponse

head :: String -> IO (Response ())
head = headWith defaults

headWith :: Options -> String -> IO (Response ())
headWith = emptyMethodWith HTTP.methodHead

put :: String -> Payload -> IO (Response ())
put = putWith defaults

putWith :: Options -> String -> Payload -> IO (Response ())
putWith opts url payload =
  request (setPayload payload . (Int.method .~ HTTP.methodPost)) opts url
    ignoreResponse

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
  let contentType = fromMaybe "unknown" . lookup "Content-Type" .
                    responseHeaders $ resp
  unless (contentType == "application/json") $
    failure (JSONError $ "content type of response is " ++ show contentType)
  case Aeson.eitherDecode' (responseBody resp) of
    Left err  -> failure (JSONError err)
    Right val -> return (fmap (const val) resp)
