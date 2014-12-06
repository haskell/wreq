{-# LANGUAGE RankNTypes, RecordWildCards #-}

module Network.Wreq.Session
    (
      Session
    , withSession
    , withSessionWith
    , withSessionWithMgr
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

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Lens ((&), (?~), (^.))
import Network.Wreq (Options, Response)
import Network.Wreq.Internal
import Network.Wreq.Internal.Types (Body(..), Req(..), Session(..))
import Network.Wreq.Types (Postable, Putable, Run)
import Prelude hiding (head)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Internal.Lens as Lens

withSession :: (Session -> IO a) -> IO a
withSession = withSessionWith defaultManagerSettings

withSessionWith :: HTTP.ManagerSettings -> (Session -> IO a) -> IO a
withSessionWith settings act = do
  mv <- newMVar $ HTTP.createCookieJar []
  HTTP.withManager settings $ \mgr ->
    act Session { seshCookies = mv
                , seshManager = mgr
                , seshRun = runWith
                }
withSessionWithMgr :: HTTP.Manager -> (Session -> IO a) -> IO a
withSessionWithMgr mgr act = do
  mv <- newMVar $ HTTP.createCookieJar []
  act Session { seshCookies = mv
              , seshManager = mgr
              , seshRun = runWith
              }

get :: Session -> String -> IO (Response L.ByteString)
get = getWith defaults

post :: Postable a => Session -> String -> a -> IO (Response L.ByteString)
post = postWith defaults

head_ :: Session -> String -> IO (Response ())
head_ = headWith defaults

options :: Session -> String -> IO (Response ())
options = optionsWith defaults

put :: Putable a => Session -> String -> a -> IO (Response L.ByteString)
put = putWith defaults

delete :: Session -> String -> IO (Response L.ByteString)
delete = deleteWith defaults

getWith :: Options -> Session -> String -> IO (Response L.ByteString)
getWith opts sesh url = run string sesh =<< prepareGet opts url

postWith :: Postable a => Options -> Session -> String -> a
         -> IO (Response L.ByteString)
postWith opts sesh url payload =
  run string sesh =<< preparePost opts url payload

headWith :: Options -> Session -> String -> IO (Response ())
headWith opts sesh url = run ignore sesh =<< prepareHead opts url

optionsWith :: Options -> Session -> String -> IO (Response ())
optionsWith opts sesh url = run ignore sesh =<< prepareOptions opts url

putWith :: Putable a => Options -> Session -> String -> a
        -> IO (Response L.ByteString)
putWith opts sesh url payload = run string sesh =<< preparePut opts url payload

deleteWith :: Options -> Session -> String -> IO (Response L.ByteString)
deleteWith opts sesh url = run string sesh =<< prepareDelete opts url

runWith :: Session -> Run Body -> Run Body
runWith Session{..} act (Req _ req) =
  modifyMVar seshCookies $ \cj -> do
    resp <- act (Req (Right seshManager) (req & Lens.cookieJar ?~ cj))
    return (resp ^. Wreq.responseCookieJar, resp)

type Mapping a = (Body -> a, a -> Body, Run a)

run :: Mapping a -> Session -> Run a
run (to,from,act) sesh =
  fmap (fmap to) . seshRun sesh sesh (fmap (fmap from) . act)

string :: Mapping L.ByteString
string = (\(StringBody s) -> s, StringBody, runRead)

ignore :: Mapping ()
ignore = (\_ -> (), const NoBody, runIgnore)
