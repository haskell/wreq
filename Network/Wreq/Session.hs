{-# LANGUAGE RankNTypes, RecordWildCards #-}

-- |
-- Module      : Network.Wreq.Session
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The functions in this module use a 'Session' to handle the
-- following common needs:
--
-- * TCP connection reuse.  This is important for performance when
--   multiple requests go to a single server, particularly if TLS is
--   being used.
--
-- * Transparent cookie management.  Any cookies set by the server
--   persist from one request to the next.  (Bypass this overhead
--   using 'withAPISession'.)
--
--
-- This module is designed to be used alongside the "Network.Wreq"
-- module.  Typical usage will look like this:
--
-- @
-- import "Network.Wreq"
-- import qualified "Network.Wreq.Session" as Sess
--
-- main = Sess.'withSession' $ \\sess ->
--   Sess.'get' sess \"http:\/\/httpbin.org\/get\"
-- @
--
-- We create a 'Session' using 'withSession', then pass the session to
-- subsequent functions.  When talking to a REST-like service that does
-- not use cookies, it is more efficient to use 'withAPISession'.
--
-- Note the use of qualified import statements in the examples above,
-- so that we can refer unambiguously to the 'Session'-specific
-- implementation of HTTP GET.
--
-- One 'Network.HTTP.Client.Manager' (possibly set with 'withSessionControl') is used for all
-- session requests. The manager settings in the 'Options' parameter
-- for the 'getWith', 'postWith' and similar functions is ignored.

module Network.Wreq.Session
    (
    -- * Session creation
      Session
    , newSession
    , newAPISession
    , withSession
    , withAPISession
    -- ** More control-oriented session creation
    , newSessionControl
    , withSessionWith
    , withSessionControl
    -- ** Get information about session state
    , getSessionCookieJar
    -- * HTTP verbs
    , get
    , post
    , head_
    , options
    , put
    , delete
    , customMethod
    -- ** Configurable verbs
    , getWith
    , postWith
    , headWith
    , optionsWith
    , putWith
    , deleteWith
    , customMethodWith
    , customPayloadMethodWith
    , customHistoriedMethodWith
    , customHistoriedPayloadMethodWith
    -- * Extending a session
    , Lens.seshRun
    ) where

import Control.Lens ((&), (.~))
import Data.Foldable (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.Wreq (Options, Response, HistoriedResponse)
import Network.Wreq.Internal
import Network.Wreq.Internal.Types (Body(..), Req(..), Session(..), RunHistory)
import Network.Wreq.Types (Postable, Putable, Run)
import Prelude hiding (head)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq.Internal.Lens as Lens
import qualified Network.Wreq.Lens as Lens
import Data.Traversable as T

-- | Create a 'Session', passing it to the given function.  The
-- 'Session' will no longer be valid after that function returns.
--
-- This session manages cookies and uses default session manager
-- configuration.
withSession :: (Session -> IO a) -> IO a
withSession act = newSession >>= act
{-# DEPRECATED withSession "Use newSession instead." #-}

-- | Create a 'Session'.
--
-- This session manages cookies and uses default session manager
-- configuration.
--
-- @since 0.5.2.0
newSession :: IO Session
newSession = newSessionControl (Just (HTTP.createCookieJar [])) defaultManagerSettings

-- | Create a session.
--
-- This uses the default session manager settings, but does not manage
-- cookies.  It is intended for use with REST-like HTTP-based APIs,
-- which typically do not use cookies.
withAPISession :: (Session -> IO a) -> IO a
withAPISession act = newAPISession >>= act
{-# DEPRECATED withAPISession "Use newAPISession instead." #-}

-- | Create a session.
--
-- This uses the default session manager settings, but does not manage
-- cookies.  It is intended for use with REST-like HTTP-based APIs,
-- which typically do not use cookies.
--
-- @since 0.5.2.0
newAPISession :: IO Session
newAPISession = newSessionControl Nothing defaultManagerSettings

-- | Create a session, using the given manager settings.  This session
-- manages cookies.
withSessionWith :: HTTP.ManagerSettings -> (Session -> IO a) -> IO a
withSessionWith = withSessionControl (Just (HTTP.createCookieJar []))
{-# DEPRECATED withSessionWith "Use newSessionControl instead." #-}

-- | Create a session, using the given cookie jar and manager settings.
withSessionControl :: Maybe HTTP.CookieJar
                  -- ^ If 'Nothing' is specified, no cookie management
                  -- will be performed.
               -> HTTP.ManagerSettings
               -> (Session -> IO a) -> IO a
withSessionControl mj settings act = do
    sess <- newSessionControl mj settings
    act sess
{-# DEPRECATED withSessionControl "Use newSessionControl instead." #-}

-- | Create a session, using the given cookie jar and manager settings.
--
-- @since 0.5.2.0
newSessionControl ::  Maybe HTTP.CookieJar
                  -- ^ If 'Nothing' is specified, no cookie management
                  -- will be performed.
               -> HTTP.ManagerSettings
               -> IO Session
newSessionControl mj settings = do
     mref <- maybe (return Nothing) (fmap Just . newIORef) mj
     mgr <- HTTP.newManager settings
     return Session { seshCookies = mref
                     , seshManager = mgr
                     , seshRun = runWith
                     , seshRunHistory = runWithHistory
                     }

-- | Extract current 'Network.HTTP.Client.CookieJar' from a 'Session'
--
-- @since 0.5.2.0
getSessionCookieJar :: Session -> IO (Maybe HTTP.CookieJar)
getSessionCookieJar = T.traverse readIORef . seshCookies

-- | 'Session'-specific version of 'Network.Wreq.get'.
get :: Session -> String -> IO (Response L.ByteString)
get = getWith defaults

-- | 'Session'-specific version of 'Network.Wreq.post'.
post :: Postable a => Session -> String -> a -> IO (Response L.ByteString)
post = postWith defaults

-- | 'Session'-specific version of 'Network.Wreq.head_'.
head_ :: Session -> String -> IO (Response ())
head_ = headWith (defaults & Lens.redirects .~ 0)

-- | 'Session'-specific version of 'Network.Wreq.options'.
options :: Session -> String -> IO (Response ())
options = optionsWith defaults

-- | 'Session'-specific version of 'Network.Wreq.put'.
put :: Putable a => Session -> String -> a -> IO (Response L.ByteString)
put = putWith defaults

-- | 'Session'-specific version of 'Network.Wreq.delete'.
delete :: Session -> String -> IO (Response L.ByteString)
delete = deleteWith defaults

-- | 'Session'-specific version of 'Network.Wreq.customMethod'.
customMethod :: String -> Session -> String -> IO (Response L.ByteString)
customMethod = flip customMethodWith defaults

-- | 'Session'-specific version of 'Network.Wreq.getWith'.
getWith :: Options -> Session -> String -> IO (Response L.ByteString)
getWith opts sesh url = run string sesh =<< prepareGet opts url

-- | 'Session'-specific version of 'Network.Wreq.postWith'.
postWith :: Postable a => Options -> Session -> String -> a
         -> IO (Response L.ByteString)
postWith opts sesh url payload =
  run string sesh =<< preparePost opts url payload

-- | 'Session'-specific version of 'Network.Wreq.headWith'.
headWith :: Options -> Session -> String -> IO (Response ())
headWith opts sesh url = run ignore sesh =<< prepareHead opts url

-- | 'Session'-specific version of 'Network.Wreq.optionsWith'.
optionsWith :: Options -> Session -> String -> IO (Response ())
optionsWith opts sesh url = run ignore sesh =<< prepareOptions opts url

-- | 'Session'-specific version of 'Network.Wreq.putWith'.
putWith :: Putable a => Options -> Session -> String -> a
        -> IO (Response L.ByteString)
putWith opts sesh url payload = run string sesh =<< preparePut opts url payload

-- | 'Session'-specific version of 'Network.Wreq.deleteWith'.
deleteWith :: Options -> Session -> String -> IO (Response L.ByteString)
deleteWith opts sesh url = run string sesh =<< prepareDelete opts url

-- | 'Session'-specific version of 'Network.Wreq.customMethodWith'.
customMethodWith :: String -> Options -> Session -> String -> IO (Response L.ByteString)
customMethodWith method opts sesh url = run string sesh =<< prepareMethod methodBS opts url
  where
    methodBS = BC8.pack method

-- | 'Session'-specific version of 'Network.Wreq.customHistoriedMethodWith'.
--
-- @since 0.5.2.0
customHistoriedMethodWith :: String -> Options -> Session -> String -> IO (HistoriedResponse L.ByteString)
customHistoriedMethodWith method opts sesh url =
    runHistory stringHistory sesh =<< prepareMethod methodBS opts url
  where
    methodBS = BC8.pack method

-- | 'Session'-specific version of 'Network.Wreq.customPayloadMethodWith'.
customPayloadMethodWith :: Postable a => String -> Options -> Session -> String -> a
                        -> IO (Response L.ByteString)
customPayloadMethodWith method opts sesh url payload =
  run string sesh =<< preparePayloadMethod methodBS opts url payload
  where
    methodBS = BC8.pack method

-- | 'Session'-specific version of 'Network.Wreq.customHistoriedPayloadMethodWith'.
--
-- @since 0.5.2.0
customHistoriedPayloadMethodWith :: Postable a => String -> Options -> Session -> String -> a
                        -> IO (HistoriedResponse L.ByteString)
customHistoriedPayloadMethodWith method opts sesh url payload =
  runHistory stringHistory sesh =<< preparePayloadMethod methodBS opts url payload
  where
    methodBS = BC8.pack method


runWithGeneric :: (resp -> Response b) -> Session -> (Req -> IO resp) -> Req -> IO resp
runWithGeneric extract Session{..} act (Req _ req) = do
  req' <- (\c -> req & Lens.cookieJar .~ c) `fmap` T.traverse readIORef seshCookies
  resp <- act (Req (Right seshManager) req')
  forM_ seshCookies $ \ref ->
    writeIORef ref (HTTP.responseCookieJar (extract resp))
  return resp

runWith :: Session -> Run Body -> Run Body
runWith = runWithGeneric id

runWithHistory :: Session -> RunHistory Body -> RunHistory Body
runWithHistory = runWithGeneric HTTP.hrFinalResponse

type Mapping a = (Body -> a, a -> Body, Run a)
type MappingHistory a = (Body -> a, a -> Body, RunHistory a)

run :: Mapping a -> Session -> Run a
run (to,from,act) sesh =
  fmap (fmap to) . seshRun sesh sesh (fmap (fmap from) . act)

runHistory :: MappingHistory a -> Session -> RunHistory a
runHistory (to,from,act) sesh =
  fmap (fmap to) . seshRunHistory sesh sesh (fmap (fmap from) . act)

string :: Mapping L.ByteString
string = (\(StringBody s) -> s, StringBody, runRead)

stringHistory :: MappingHistory L.ByteString
stringHistory = (\(StringBody s) -> s, StringBody, runReadHistory)

ignore :: Mapping ()
ignore = (const (), const NoBody, runIgnore)
