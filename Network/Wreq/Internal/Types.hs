{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances, GADTs,
    OverloadedStrings, RankNTypes, RecordWildCards #-}

-- |
-- Module      : Network.Wreq.Internal.Types
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client types.

module Network.Wreq.Internal.Types
    (
    -- * Client configuration
      Options(..)
    , Mgr
    , Auth(..)
    , AWSAuthVersion(..)
    , ResponseChecker
    -- * Request payloads
    , Payload(..)
    , Postable(..)
    , Putable(..)
    -- ** URL-encoded forms
    , FormParam(..)
    , FormValue(..)
    -- * Headers
    , ContentType
    , Link(..)
    -- * Errors
    , JSONError(..)
    -- * Request types
    , Req(..)
    , reqURL
    -- * Sessions
    , Session(..)
    , Run
    , Body(..)
    -- * Caches
    , CacheEntry(..)
    ) where

import Control.Exception (Exception)
import Data.IORef (IORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Network.HTTP.Client (CookieJar, Manager, ManagerSettings, Request,
                            RequestBody)
import Network.HTTP.Client.Internal (Response, Proxy)
import Network.HTTP.Types (Header)
import Prelude hiding (head)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP

-- | A MIME content type, e.g. @\"application/octet-stream\"@.
type ContentType = S.ByteString

type Mgr = Either ManagerSettings Manager

-- | Options for configuring a client.
data Options = Options {
    manager :: Mgr
  -- ^ Either configuration for a 'Manager', or an actual 'Manager'.
  --
  -- If only 'ManagerSettings' are provided, then by default a new
  -- 'Manager' will be created for each request.
  --
  -- /Note/: when issuing HTTP requests using 'Options'-based
  -- functions from the the "Network.Wreq.Session" module
  -- ('Network.Wreq.Session.getWith', 'Network.Wreq.Session.putWith',
  -- etc.), this field will be ignored.
  --
  -- An example of using a specific manager:
  --
  -- @
  --import "Network.HTTP.Client" ('Network.HTTP.Client.withManager')
  --
  --'Network.HTTP.Client.withManager' $ \\mgr -> do
  --  let opts = 'Network.Wreq.defaults' { 'manager' = Right mgr }
  --  'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
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
  --    opts = 'Network.Wreq.defaults' { 'manager' = Left settings }
  --'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
  -- @
  , proxy :: Maybe Proxy
  -- ^ Host name and port for a proxy to use, if any.
  , auth :: Maybe Auth
  -- ^ Authentication information.
  --
  -- Example (note the use of TLS):
  --
  -- @
  --let opts = 'Network.Wreq.defaults' { 'auth' = 'Network.Wreq.basicAuth' \"user\" \"pass\" }
  --'Network.Wreq.getWith' opts \"https:\/\/httpbin.org\/basic-auth\/user\/pass\"
  -- @
  , headers :: [Header]
  -- ^ Additional headers to send with each request.
  --
  -- @
  --let opts = 'Network.Wreq.defaults' { 'headers' = [(\"Accept\", \"*\/*\")] }
  --'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
  -- @
  , params :: [(Text, Text)]
  -- ^ Key-value pairs to assemble into a query string to add to the
  -- end of a URL.
  --
  -- For example, given:
  --
  -- @
  --let opts = 'Network.Wreq.defaults' { params = [(\"sort\", \"ascending\"), (\"key\", \"name\")] }
  --'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/get\"
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
  --let opts = 'Network.Wreq.defaults' { 'redirects' = 3 }
  --'Network.Wreq.getWith' opts \"http:\/\/httpbin.org\/redirect/5\"
  -- @
  , cookies :: Maybe CookieJar
  -- ^ Cookies to set when issuing requests.
  --
  -- /Note/: when issuing HTTP requests using 'Options'-based
  -- functions from the the "Network.Wreq.Session" module
  -- ('Network.Wreq.Session.getWith', 'Network.Wreq.Session.putWith',
  -- etc.), this field will be used only for the /first/ HTTP request
  -- to be issued during a 'Network.Wreq.Session.Session'. Any changes
  -- changes made for subsequent requests will be ignored.
  , checkResponse :: Maybe ResponseChecker
  -- ^ Function that checks the status code and potentially returns an
  -- exception.
  --
  -- This defaults to 'Nothing', which will just use the default of
  -- 'Network.HTTP.Client.Request' which throws a 'StatusException' if
  -- the status is not 2XX.
  } deriving (Typeable)

-- | A function that checks the result of a HTTP request and
-- potentially returns an exception.
type ResponseChecker = Request -> Response HTTP.BodyReader -> IO ()

-- | Supported authentication types.
--
-- Do not use HTTP authentication unless you are using TLS encryption.
-- These authentication tokens can easily be captured and reused by an
-- attacker if transmitted in the clear.
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
          | AWSAuth AWSAuthVersion S.ByteString S.ByteString
            -- ^ Amazon Web Services request signing
            -- AWSAuthVersion key secret
          | OAuth1 S.ByteString S.ByteString S.ByteString S.ByteString
            -- ^ OAuth1 request signing
            -- OAuth1 consumerToken consumerSecret token secret
          deriving (Eq, Show, Typeable)

data AWSAuthVersion = AWSv4
                      -- ^ AWS request signing version 4
                    deriving (Eq, Show)

instance Show Options where
  show (Options{..}) = concat [
      "Options { "
    , "manager = ", case manager of
                      Left _  -> "Left _"
                      Right _ -> "Right _"
    , ", proxy = ", show proxy
    , ", auth = ", show auth
    , ", headers = ", show headers
    , ", params = ", show params
    , ", redirects = ", show redirects
    , ", cookies = ", show cookies
    , " }"
    ]

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

-- | A type that can be rendered as the value portion of a key\/value
-- pair for use in an @application\/x-www-form-urlencoded@ POST
-- body. Intended for use with the 'FormParam' type.
--
-- The instances for 'String', strict 'Data.Text.Text', and lazy
-- 'Data.Text.Lazy.Text' are all encoded using UTF-8 before being
-- URL-encoded.
--
-- The instance for 'Maybe' gives an empty string on 'Nothing',
-- and otherwise uses the contained type's instance.
class FormValue a where
    renderFormValue :: a -> S.ByteString
    -- ^ Render the given value.

-- | A key\/value pair for an @application\/x-www-form-urlencoded@
-- POST request body.
data FormParam where
    (:=) :: (FormValue v) => S.ByteString -> v -> FormParam

instance Show FormParam where
    show (a := b) = show a ++ " := " ++ show (renderFormValue b)

infixr 3 :=

-- | The error type used by 'Network.Wreq.asJSON' and
-- 'Network.Wreq.asValue' if a failure occurs when parsing a response
-- body as JSON.
data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

-- | An element of a @Link@ header.
data Link = Link {
      linkURL :: S.ByteString
    , linkParams :: [(S.ByteString, S.ByteString)]
    } deriving (Eq, Show, Typeable)

-- | A request that is ready to be submitted.
data Req = Req Mgr Request

-- | Return the URL associated with the given 'Req'.
--
-- This includes the port number if not standard, and the query string
-- if one exists.
reqURL :: Req -> S.ByteString
reqURL (Req _ req) = mconcat [
    if https then "https" else "http"
  , "://"
  , HTTP.host req
  , case (HTTP.port req, https) of
      (80, False) -> ""
      (443, True) -> ""
      (p, _)      -> S.pack (show p)
  , HTTP.path req
  , case HTTP.queryString req of
      qs | S.null qs -> ""
         | otherwise -> "?" <> qs
  ]
  where https = HTTP.secure req

-- | A function that runs a request and returns the associated
-- response.
type Run body = Req -> IO (Response body)

-- | A session that spans multiple requests.  This is responsible for
-- cookie management and TCP connection reuse.
data Session = Session {
      seshCookies :: Maybe (IORef CookieJar)
    , seshManager :: Manager
    , seshRun :: Session -> Run Body -> Run Body
    }

instance Show Session where
    show _ = "Session"

data CacheEntry body = CacheEntry {
    entryCreated  :: UTCTime
  , entryExpires  :: Maybe UTCTime
  , entryResponse :: Response body
  } deriving (Functor)

data Body = NoBody
          | StringBody L.ByteString
          | ReaderBody HTTP.BodyReader

instance Show (CacheEntry body) where
    show _ = "CacheEntry"
