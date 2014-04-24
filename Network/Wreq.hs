{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- |
-- Module      : Network.Wreq
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- A library for client-side HTTP requests, focused on ease of use.
--
-- When reading the examples in this module, you should assume the
-- following environment:
--
-- @
-- \-\- Make it easy to write literal 'S.ByteString' and 'Text' values.
-- \{\-\# LANGUAGE OverloadedStrings \#\-\}
--
-- \-\- Our handy module.
-- import "Network.Wreq"
--
-- \-\- Operators such as ('&') and ('.~').
-- import "Control.Lens"
--
-- \-\- Conversion of Haskell values to JSON.
-- import "Data.Aeson" ('Data.Aeson.toJSON')
--
-- \-\- Easy traversal of JSON data.
-- import "Data.Aeson.Lens" ('Data.Aeson.Lens.key', 'Data.Aeson.Lens.nth')
-- @
--
-- There exist some less frequently used lenses that are not exported
-- from this module; these can be found in "Network.Wreq.Lens".

module Network.Wreq
    (
    -- * HTTP verbs
    -- ** GET
      get
    , getWith
    -- ** POST
    -- $postable
    , post
    , postWith
    -- ** HEAD
    , head_
    , headWith
    -- ** OPTIONS
    , options
    , optionsWith
    -- ** PUT
    , put
    , putWith
    -- ** DELETE
    , delete
    , deleteWith
    -- * Incremental consumption of responses
    -- ** GET
    , foldGet
    , foldGetWith

    -- * Configuration
    , Options
    , defaults
    , Lens.manager
    , Lens.header
    , Lens.param
    , Lens.redirects
    , Lens.headers
    , Lens.params
    , Lens.cookie
    , Lens.cookies
    -- ** Authentication
    -- $auth
    , Auth
    , Lens.auth
    , basicAuth
    , oauth2Bearer
    , oauth2Token
    -- ** Proxy settings
    , Proxy(Proxy)
    , Lens.proxy
    , httpProxy
    -- ** Using a manager with defaults
    , withManager

    -- * Payloads for POST and PUT
    , Payload(..)
    -- ** URL-encoded form data
    , FormParam(..)
    , FormValue
    -- ** Multipart form data
    , Form.Part
    , Lens.partName
    , Lens.partFileName
    , Lens.partContentType
    , Lens.partGetBody
    -- *** Smart constructors
    , Form.partBS
    , Form.partLBS
    , partText
    , partString
    , Form.partFile
    , Form.partFileSource

    -- * Responses
    , Response
    , Lens.responseBody
    , Lens.responseHeader
    , Lens.responseLink
    , Lens.responseCookie
    , Lens.responseHeaders
    , Lens.responseCookieJar
    , Lens.responseStatus
    , Lens.Status
    , Lens.statusCode
    -- ** Link headers
    , Lens.Link
    , Lens.linkURL
    , Lens.linkParams
    -- ** Decoding responses
    , JSONError(..)
    , asJSON
    , asValue

    -- * Cookies
    -- $cookielenses
    , Lens.Cookie
    , Lens.cookieName
    , Lens.cookieValue
    , Lens.cookieExpiryTime
    , Lens.cookieDomain
    , Lens.cookiePath

    -- * Parsing responses
    , Lens.atto
    ) where

import Control.Lens ((.~), (&))
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Aeson (FromJSON)
import Data.ByteString.Char8 ()
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client.Internal (Proxy(..), Response)
import Network.Wreq.Internal
import Network.Wreq.Types (Options)
import Network.Wreq.Types hiding (Options(..))
import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wreq.Internal.Lens as Int
import qualified Network.Wreq.Lens as Lens
import qualified Network.Wreq.Types as Wreq

-- | Issue a GET request.
--
-- Example:
--
-- @
--'get' \"http:\/\/httpbin.org\/get\"
-- @
--
-- >>> r <- get "http://httpbin.org/get"
-- >>> r ^. responseStatus . statusCode
-- 200
get :: String -> IO (Response L.ByteString)
get url = getWith defaults url

withManager :: (Options -> IO a) -> IO a
withManager act = HTTP.withManager defaultManagerSettings $ \mgr ->
  act defaults { Wreq.manager = Right mgr }

-- | Issue a GET request, using the supplied 'Options'.
--
-- Example:
--
-- @
--let opts = 'defaults' '&' 'Lens.param' \"foo\" '.~' [\"bar\"]
--'getWith' opts \"http:\/\/httpbin.org\/get\"
-- @
--
-- >>> let opts = defaults & param "foo" .~ ["bar"]
-- >>> r <- getWith opts "http://httpbin.org/get"
-- >>> r ^? responseBody . key "url"
-- Just (String "http://httpbin.org/get?foo=bar")
getWith :: Options -> String -> IO (Response L.ByteString)
getWith opts url = request id opts url readResponse

-- | Issue a POST request.
--
-- Example:
--
-- @
--'post' \"http:\/\/httpbin.org\/post\" ('Aeson.toJSON' [1,2,3])
-- @
--
-- >>> r <- post "http://httpbin.org/post" (toJSON [1,2,3])
-- >>> r ^? responseBody . key "json"
-- Just (Array (fromList [Number 1.0,Number 2.0,Number 3.0]))
post :: Postable a => String -> a -> IO (Response L.ByteString)
post url payload = postWith defaults url payload

-- | Issue a POST request, using the supplied 'Options'.
--
-- Example:
--
-- @
--let opts = 'defaults' '&' 'Lens.param' \"foo\" '.~' [\"bar\"]
--'postWith' opts \"http:\/\/httpbin.org\/post\" ('Aeson.toJSON' [1,2,3])
-- @
--
-- >>> let opts = defaults & param "foo" .~ ["bar"]
-- >>> r <- postWith opts "http://httpbin.org/post" (toJSON [1,2,3])
-- >>> r ^? responseBody . key "url"
-- Just (String "http://httpbin.org/post?foo=bar")
postWith :: Postable a => Options -> String -> a -> IO (Response L.ByteString)
postWith opts url payload =
  requestIO (postPayload payload . (Int.method .~ HTTP.methodPost)) opts url
    readResponse

-- | Issue a HEAD request.
--
-- Example:
--
-- @
--'head_' \"http:\/\/httpbin.org\/get\"
-- @
--
-- >>> r <- head_ "http://httpbin.org/get"
-- >>> r ^? responseHeader "Content-Type"
-- Just "application/json"
head_ :: String -> IO (Response ())
head_ = headWith (defaults & Lens.redirects .~ 0)

-- | Issue a HEAD request, using the supplied 'Options'.
--
-- Example:
--
-- @
--let opts = 'defaults' '&' 'Lens.param' \"foo\" '.~' [\"bar\"]
--'headWith' opts \"http:\/\/httpbin.org\/get\"
-- @
--
-- >>> let opts = defaults & param "foo" .~ ["bar"]
-- >>> r <- headWith opts "http://httpbin.org/get"
-- >>> r ^? responseHeader "Connection"
-- Just "keep-alive"
headWith :: Options -> String -> IO (Response ())
headWith = emptyMethodWith HTTP.methodHead

-- | Issue a PUT request.
put :: Putable a => String -> a -> IO (Response L.ByteString)
put url payload = putWith defaults url payload

-- | Issue a PUT request, using the supplied 'Options'.
putWith :: Putable a => Options -> String -> a -> IO (Response L.ByteString)
putWith opts url payload =
  requestIO (putPayload payload . (Int.method .~ HTTP.methodPut)) opts url
    readResponse

-- | Issue an OPTIONS request.
--
-- Example:
--
-- @
--'options' \"http:\/\/httpbin.org\/get\"
-- @
--
-- See 'Lens.atto' for a more complex worked example.
options :: String -> IO (Response ())
options = optionsWith defaults

-- | Issue an OPTIONS request, using the supplied 'Options'.
--
-- Example:
--
-- @
--let opts = 'defaults' '&' 'Lens.param' \"foo\" '.~' [\"bar\"]
--'optionsWith' opts \"http:\/\/httpbin.org\/get\"
-- @
optionsWith :: Options -> String -> IO (Response ())
optionsWith = emptyMethodWith HTTP.methodOptions

-- | Issue a DELETE request.
--
-- Example:
--
-- @
--'delete' \"http:\/\/httpbin.org\/delete\"
-- @
--
-- >>> r <- delete "http://httpbin.org/delete"
-- >>> r ^. responseStatus . statusCode
-- 200
delete :: String -> IO (Response ())
delete = deleteWith defaults

-- | Issue a DELETE request, using the supplied 'Options'.
--
-- Example:
--
-- @
--let opts = 'defaults' '&' 'Lens.redirects' '.~' 0
--'deleteWith' opts \"http:\/\/httpbin.org\/delete\"
-- @
--
-- >>> let opts = defaults & redirects .~ 0
-- >>> r <- deleteWith opts "http://httpbin.org/delete"
-- >>> r ^. responseStatus . statusCode
-- 200
deleteWith :: Options -> String -> IO (Response ())
deleteWith = emptyMethodWith HTTP.methodDelete

foldGet :: (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGet f z url = foldGetWith defaults f z url

foldGetWith :: Options -> (a -> S.ByteString -> IO a) -> a -> String -> IO a
foldGetWith opts f z0 url = request id opts url (foldResponseBody f z0)

-- | Convert the body of an HTTP response from JSON to a suitable
-- Haskell type.
--
-- In this example, we use 'asJSON' in the @IO@ monad, where it will
-- throw a 'JSONError' exception if conversion to the desired type
-- fails.
--
-- @
-- \{-\# LANGUAGE DeriveGeneric \#-\}
--import "GHC.Generics" ('GHC.Generics.Generic')
--
-- \{- This Haskell type corresponds to the structure of a
--   response body from httpbin.org. -\}
--
--data GetBody = GetBody {
--    headers :: 'Data.Map.Map' 'Data.Text.Text' 'Data.Text.Text'
--  , args :: 'Data.Map.Map' 'Data.Text.Text' 'Data.Text.Text'
--  , origin :: 'Data.Text.Text'
--  , url :: 'Data.Text.Text'
--  } deriving (Show, 'GHC.Generics.Generic')
--
-- \-\- Get GHC to derive a 'FromJSON' instance for us.
--instance 'FromJSON' GetBody
--
-- \{- The fact that we want a GetBody below will be inferred by our
--   use of the \"headers\" accessor function. -\}
--
--foo = do
--  r <- 'asJSON' =<< 'get' \"http:\/\/httpbin.org\/get\"
--  print (headers (r 'Control.Lens.^.' 'responseBody'))
-- @
--
-- If we use 'asJSON' in the 'Either' monad, it will return 'Left'
-- with a 'JSONError' payload if conversion fails, and 'Right' with a
-- 'Response' whose 'responseBody' is the converted value on success.

asJSON :: (MonadThrow m, FromJSON a) =>
          Response L.ByteString -> m (Response a)
{-# SPECIALIZE asJSON :: (FromJSON a) =>
                         Response L.ByteString -> IO (Response a) #-}
{-# SPECIALIZE asJSON :: Response L.ByteString -> IO (Response Aeson.Value) #-}
asJSON resp = do
  let contentType = fst . S.break (==59) . fromMaybe "unknown" .
                    lookup "Content-Type" . HTTP.responseHeaders $ resp
  unless ("application/json" `S.isPrefixOf` contentType) $
    throwM . JSONError $ "content type of response is " ++ show contentType
  case Aeson.eitherDecode' (HTTP.responseBody resp) of
    Left err  -> throwM (JSONError err)
    Right val -> return (fmap (const val) resp)


-- | Convert the body of an HTTP response from JSON to a 'Value'.
--
-- In this example, we use 'asValue' in the @IO@ monad, where it will
-- throw a 'JSONError' exception if the conversion to 'Value' fails.
--
-- @
--foo = do
--  r <- 'asValue' =<< 'get' \"http:\/\/httpbin.org\/get\"
--  print (r 'Control.Lens.^?' 'responseBody' . key \"headers\" . key \"User-Agent\")
-- @
asValue :: (MonadThrow m) => Response L.ByteString -> m (Response Aeson.Value)
{-# SPECIALIZE asValue :: Response L.ByteString
                       -> IO (Response Aeson.Value) #-}
asValue = asJSON

-- $auth
--
-- Do not use HTTP authentication unless you are using TLS encryption.
-- These authentication tokens can easily be captured and reused by an
-- attacker if transmitted in the clear.

-- | Basic authentication. This consists of a plain username and
-- password.
--
-- Example (note the use of TLS):
--
-- @
--let opts = 'defaults' '&' 'Lens.auth' '?~' 'basicAuth' \"user\" \"pass\"
--'getWith' opts \"https:\/\/httpbin.org\/basic-auth\/user\/pass\"
-- @
--
-- Note here the use of the 'Control.Lens.?~' setter to turn an 'Auth'
-- into a 'Maybe' 'Auth', to make the type of the RHS compatible with
-- the 'Lens.auth' lens.
--
-- >>> let opts = defaults & auth ?~ basicAuth "user" "pass"
-- >>> r <- getWith opts "https://httpbin.org/basic-auth/user/pass"
-- >>> r ^? responseBody . key "authenticated"
-- Just (Bool True)
basicAuth :: S.ByteString       -- ^ Username.
          -> S.ByteString       -- ^ Password.
          -> Auth
basicAuth = BasicAuth

-- | An OAuth2 bearer token. This is treated by many services as the
-- equivalent of a username and password.
--
-- Example (note the use of TLS):
--
-- @
--let opts = 'defaults' '&' 'Lens.auth' '?~' 'oauth2Bearer' \"1234abcd\"
--'getWith' opts \"https:\/\/public-api.wordpress.com\/rest\/v1\/me\/\"
-- @
oauth2Bearer :: S.ByteString -> Auth
oauth2Bearer = OAuth2Bearer

-- | A not-quite-standard OAuth2 bearer token (that seems to be used
-- only by GitHub). This will be treated by whatever services accept
-- it as the equivalent of a username and password.
--
-- Example (note the use of TLS):
--
-- @
--let opts = 'defaults' '&' 'Lens.auth' '?~' 'oauth2Token' \"abcd1234\"
--'getWith' opts \"https:\/\/api.github.com\/user\"
-- @
oauth2Token :: S.ByteString -> Auth
oauth2Token = OAuth2Token

-- | Proxy configuration.
--
-- Example:
--
-- @
--let opts = 'defaults' '&' 'Lens.proxy' '?~' 'httpProxy' \"localhost\" 8000
--'getWith' opts \"http:\/\/httpbin.org\/get\"
-- @
--
-- Note here the use of the 'Control.Lens.?~' setter to turn a 'Proxy'
-- into a 'Maybe' 'Proxy', to make the type of the RHS compatible with
-- the 'Lens.proxy' lens.
httpProxy :: S.ByteString -> Int -> Proxy
httpProxy = Proxy

-- | Make a 'Part' whose content is a strict 'T.Text', encoded as
-- UTF-8.
--
-- The 'Part' does not have a file name or content type associated
-- with it.
partText :: Text             -- ^ Name of the corresponding \<input\>.
         -> Text             -- ^ The body for this 'Form.Part'.
         -> Form.Part
partText name value = Form.partBS name (encodeUtf8 value)

-- | Make a 'Part' whose content is a 'String', encoded as UTF-8.
--
-- The 'Part' does not have a file name or content type associated
-- with it.
partString :: Text           -- ^ Name of the corresponding \<input\>.
           -> String         -- ^ The body for this 'Form.Part'.
           -> Form.Part
partString name value = Form.partBS name (encodeUtf8 (T.pack value))

-- $cookielenses
--
-- These are only the most frequently-used cookie-related lenses.  See
-- "Network.Wreq.Lens" for the full accounting of them all.

-- $postable
--
-- The 'Postable' class determines which Haskell types can be used as
-- POST payloads.
--
-- 'Form.Part' and ['Form.Part'] give a request body with a
-- @Content-Type@ of @multipart/form-data@.  Constructor functions
-- include 'partText' and 'Form.partFile'.
--
-- >>> r <- post "http://httpbin.org/post" (partText "hello" "world")
-- >>> r ^? responseBody . key "form" . key "hello"
-- Just (String "world")
--
-- ('S.ByteString', 'S.ByteString') and 'FormParam' (and lists of
-- each) give a request body with a @Content-Type@ of
-- @application/x-www-form-urlencoded@. The easiest way to use this is
-- via the (':=') constructor.
--
-- >>> r <- post "http://httpbin.org/post" ["num" := 31337, "str" := "foo"]
-- >>> r ^? responseBody . key "form" . key "num"
-- Just (String "31337")
--
-- The \"magical\" type conversion on the right-hand side of ':='
-- above is due to the 'FormValue' class. This package provides
-- sensible instances for the standard string and number types.
--
-- The 'Aeson.Value' type gives a JSON request body with a
-- @Content-Type@ of @application/json@. Any instance of
-- 'Aeson.ToJSON' can of course be converted to a 'Aeson.Value' using
-- 'Aeson.toJSON'.
--
-- >>> r <- post "http://httpbin.org/post" (toJSON [1,2,3])
-- >>> r ^? responseBody . key "json" . nth 0
-- Just (Number 1.0)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens
-- >>> import Data.Aeson (toJSON)
-- >>> import Data.Aeson.Lens (key, nth)
-- >>> import Network.Wreq
