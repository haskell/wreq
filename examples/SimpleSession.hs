{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.Wreq
import qualified Network.Wreq.Session as S

main :: IO ()
main = S.withSession $ \sess -> do
  -- Our first request causes the httpbin.org server to set a cookie
  -- in its response.

  S.get sess "http://httpbin.org/cookies/set?name=hi"

  -- The session value manages both cookies and HTTP connection reuse
  -- for us.  When we issue the second request, it should
  -- transparently reuse the same connection, and also send the
  -- cookies that we set during the first request.

  r2 <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)]

  -- And here's where we verify that the cookie is still set on the
  -- second request.

  print $ r2 ^. responseCookie "name" . cookieValue
