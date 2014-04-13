module Network.WReq.Session
    (
      Session
    , new
    , get
    , getWith
    ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Lens
import Network.WReq (Options, Response)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.WReq as WReq

newtype Session = Session {
      seshCookies :: MVar HTTP.CookieJar
    } deriving (Eq)

instance Show Session where
    show _ = "Session"

new :: IO Session
new = do
  ref <- newMVar $ HTTP.createCookieJar []
  return Session { seshCookies = ref }

get :: Session -> String -> IO (Response L.ByteString)
get = getWith WReq.defaults

getWith :: Options -> Session -> String -> IO (Response L.ByteString)
getWith opts sesh url =
  modifyMVar (seshCookies sesh) $ \cj -> do
    resp <- WReq.getWith (opts & WReq.cookies .~ cj) url
    return (resp ^. WReq.responseCookieJar, resp)
