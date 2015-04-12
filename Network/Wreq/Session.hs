{-# LANGUAGE RankNTypes, RecordWildCards #-}

-- |
-- Module      : Network.Wreq
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
--   persist from one request to the next.
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
-- subsequent functions.
--
-- Note the use of a qualified import statement, so that we can refer
-- unambiguously to the 'Session'-specific implementation of HTTP GET.

module Network.Wreq.Session
    (
      Session
    , withSession
    , withSessionWith
    -- * HTTP verbs
    , get
    , post
    , head_
    , options
    , put
    , delete
    -- ** Configurable verbs
    , getWith
    , postWith
    , headWith
    , optionsWith
    , putWith
    , deleteWith
    -- * Extending a session
    , Lens.seshRun
    ) where

import Control.Lens ((&), (?~))
import Data.IORef (newIORef, readIORef, atomicModifyIORef)
import Data.Time (getCurrentTime)
import Network.Wreq (Options, Response)
import Network.Wreq.Internal
import Network.Wreq.Internal.Types (Body(..), Req(..), Session(..))
import Network.Wreq.Types (Postable, Putable, Run)
import Prelude hiding (head)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq.Internal.Lens as Lens

-- | Create a 'Session', passing it to the given function.  The
-- 'Session' will no longer be valid after that function returns.
withSession :: (Session -> IO a) -> IO a
withSession = withSessionWith defaultManagerSettings

-- | Create a session, using the given manager settings.
withSessionWith :: HTTP.ManagerSettings -> (Session -> IO a) -> IO a
withSessionWith settings act = do
  ref <- newIORef $ HTTP.createCookieJar []
  HTTP.withManager settings $ \mgr ->
    act Session { seshCookies = ref
                , seshManager = mgr
                , seshRun = runWith
                }

-- | 'Session'-specific version of 'Network.Wreq.get'.
get :: Session -> String -> IO (Response L.ByteString)
get = getWith defaults

-- | 'Session'-specific version of 'Network.Wreq.post'.
post :: Postable a => Session -> String -> a -> IO (Response L.ByteString)
post = postWith defaults

-- | 'Session'-specific version of 'Network.Wreq.head_'.
head_ :: Session -> String -> IO (Response ())
head_ = headWith defaults

-- | 'Session'-specific version of 'Network.Wreq.options'.
options :: Session -> String -> IO (Response ())
options = optionsWith defaults

-- | 'Session'-specific version of 'Network.Wreq.put'.
put :: Putable a => Session -> String -> a -> IO (Response L.ByteString)
put = putWith defaults

-- | 'Session'-specific version of 'Network.Wreq.delete'.
delete :: Session -> String -> IO (Response L.ByteString)
delete = deleteWith defaults

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

runWith :: Session -> Run Body -> Run Body
runWith Session{..} act (Req _ req) = do
  cj <- readIORef seshCookies
  let req' = req & Lens.cookieJar ?~ cj
  resp <- act (Req (Right seshManager) req')
  now <- getCurrentTime
  atomicModifyIORef seshCookies $ \ceej ->
    case HTTP.destroyCookieJar ceej of
      [] -> (HTTP.responseCookieJar resp, resp)
      _  -> HTTP.updateCookieJar resp req' now ceej

type Mapping a = (Body -> a, a -> Body, Run a)

run :: Mapping a -> Session -> Run a
run (to,from,act) sesh =
  fmap (fmap to) . seshRun sesh sesh (fmap (fmap from) . act)

string :: Mapping L.ByteString
string = (\(StringBody s) -> s, StringBody, runRead)

ignore :: Mapping ()
ignore = (\_ -> (), const NoBody, runIgnore)
