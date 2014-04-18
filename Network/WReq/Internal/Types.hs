{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs, RecordWildCards #-}

-- |
-- Module      : Network.WReq.Internal.Types
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client types.

module Network.WReq.Internal.Types
    (
    -- * Client configuration
      Options(..)
    , Auth(..)
    -- * Request payloads
    , Payload(..)
    , Postable(..)
    , Putable(..)
    -- * Headers
    , ContentType
    , Param
    , Link(..)
    -- * Errors
    , JSONError(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Network.HTTP.Client (CookieJar, Manager, ManagerSettings, Request,
                            RequestBody, destroyCookieJar)
import Network.HTTP.Client.Internal (Proxy)
import Network.HTTP.Types (Header)
import Prelude hiding (head)
import qualified Data.ByteString as S

-- | A MIME content type, e.g. @\"application/octet-stream\"@.
type ContentType = S.ByteString

-- | Options for configuring a client.
data Options = Options {
    manager :: Either ManagerSettings Manager
  -- ^ Either configuration for a 'Manager', or an actual 'Manager'.
  --
  -- If only 'ManagerSettings' are provided, then by default a new
  -- 'Manager' will be created for each request.
  --
  -- /Note/: when issuing HTTP requests using 'Options'-based
  -- functions from the the "Network.WReq.Session" module
  -- ('Network.WReq.Session.getWith', 'Network.WReq.Session.putWith',
  -- etc.), this field will be ignored.
  --
  -- An example of using a specific manager:
  --
  -- @
  --import "Network.HTTP.Client" ('Network.HTTP.Client.withManager')
  --
  --'Network.HTTP.Client.withManager' $ \\mgr -> do
  --  let opts = 'Network.WReq.defaults' { 'manager' = Right mgr }
  --  'Network.WReq.getWith' opts \"http:\/\/httpbin.org\/get\"
  -- @
  --
  -- An example of changing settings (this will use a separate
  -- 'Manager' for every request, so make sense only if you're issuing
  -- a tiny handful of requets):
  --
  -- @
  --import "Network.HTTP.Client" ('Network.HTTP.Client.defaultManagerSettings')
  --
  --let settings = 'Network.HTTP.Client.defaultManagerSettings' { managerConnCount = 5 }
  --    opts = 'Network.WReq.defaults' { 'manager' = Left settings }
  --'Network.WReq.getWith' opts \"http:\/\/httpbin.org\/get\"
  -- @
  , proxy :: Maybe Proxy
  -- ^ Host name and port for a proxy to use, if any.
  , auth :: Maybe Auth
  -- ^ Authentication information.
  --
  -- Example:
  --
  -- @
  --let opts = 'Network.WReq.defaults' { 'auth' = 'Network.WReq.basicAuth' \"user\" \"pass\" }
  --'Network.WReq.getWith' opts \"http:\/\/httpbin.org\/basic-auth\/user\/pass\"
  -- @
  , headers :: [Header]
  -- ^ Additional headers to send with each request.
  --
  -- @
  --let opts = 'Network.WReq.defaults' { 'headers' = [(\"Accept\", \"*\/*\")] }
  --'Network.WReq.getWith' opts \"http:\/\/httpbin.org\/get\"
  -- @
  , params :: [(S.ByteString, S.ByteString)]
  -- ^ Key-value pairs to assemble into a query string to add to the
  -- end of a URL.
  --
  -- For example, given:
  --
  -- @
  --let opts = 'Network.WReq.defaults' { params = [(\"sort\", \"ascending\"), (\"key\", \"name\")] }
  --'Network.WReq.getWith' opts \"http:\/\/httpbin.org\/get\"
  -- @
  --
  -- This will generate a URL of the form:
  --
  -- >http://httpbin.org/get?sort=ascending&key=name
  , redirects :: Int
  -- ^ The maximum number of HTTP redirects to follow before giving up
  -- and throwing an exception.
  --
  -- In this example, a 'Network.HTTP.Client.HttpException' will be
  -- thrown with a 'Network.HTTP.Client.TooManyRedirects' constructor,
  -- because the maximum number of redirects allowed will be exceeded:
  --
  -- @
  --let opts = 'Network.WReq.defaults' { 'redirects' = 3 }
  --'Network.WReq.getWith' opts \"http:\/\/httpbin.org\/redirect/5\"
  -- @
  , cookies :: CookieJar
  -- ^ Cookies to set when issuing requests.
  --
  -- /Note/: when issuing HTTP requests using 'Options'-based
  -- functions from the the "Network.WReq.Session" module
  -- ('Network.WReq.Session.getWith', 'Network.WReq.Session.putWith',
  -- etc.), this field will be used only for the /first/ HTTP request
  -- to be issued during a 'Network.WReq.Session.Session'. Any changes
  -- changes made for subsequent requests will be ignored.
  } deriving (Typeable)

-- | Supported authentication types.
--
-- HTTP authentication should not be considered secure.  Do not use
-- authentication unless you are operating over TLS.
data Auth = BasicAuth S.ByteString S.ByteString
            -- ^ Basic authentication. This consists of a plain
            -- username and password.
          | OAuth2Bearer S.ByteString
            -- ^ An OAuth2 bearer token. This is treated by many
            -- services as the equivalent of a username and password.
          | OAuth2Token S.ByteString
            -- ^ A not-quite-standard OAuth2 bearer token (that seems
            -- to be used only by GitHub). This is treated by whoever
            -- accepts it as the equivalent of a username and
            -- password.
          deriving (Eq, Show, Typeable)

instance Show Options where
  show (Options{..}) = concat ["Options { "
                              , "manager = ", case manager of
                                                Left _  -> "Left _"
                                                Right _ -> "Right _"
                              , ", proxy = ", show proxy
                              , ", auth = ", show auth
                              , ", headers = ", show headers
                              , ", params = ", show params
                              , ", redirects = ", show redirects
                              , ", cookies = ", show (destroyCookieJar cookies)
                              , " }"
                              ]

-- | A key\/value pair.
type Param = (S.ByteString, S.ByteString)

-- | A type that can be converted into a POST request payload.
class Postable a where
    postPayload :: a -> Request -> IO Request
    -- ^ Represent a value in the request body (and perhaps the
    -- headers) of a POST request.

-- | A type that can be converted into a PUT request payload.
class Putable a where
    putPayload :: a -> Request -> IO Request
    -- ^ Represent a value in the request body (and perhaps the
    -- headers) of a PUT request.

-- | A product type for representing more complex payload types.
data Payload where
    Raw  :: ContentType -> RequestBody -> Payload
  deriving (Typeable)

-- | The error type used by 'Network.WReq.asJSON' and
-- 'Network.WReq.asValue' if a failure occurs when parsing a response
-- body as JSON.
data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

-- | An element of a @Link:@ header.
data Link = Link {
      linkURL :: S.ByteString
    , linkParams :: [Param]
    } deriving (Eq, Show, Typeable)
