{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-missing-signatures
    -fno-warn-unused-binds #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception)
import Control.Lens ((^.), (^?), (.~), (?~), (&))
import Control.Monad (unless, void)
import Data.Aeson (Value(..), object)
import Data.Aeson.Lens (key)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import HttpBin.Server (serve)
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Status (status200, status401)
import Network.HTTP.Types.Version (http11)
import Network.Wreq
import Network.Wreq.Lens
import Snap.Http.Server.Config
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual, assertFailure)
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Network.Wreq.Session as Session

basicGet site = do
  r <- get (site "/get")
  assertBool "GET request has User-Agent header" $
    isJust (r ^. responseBody ^? key "headers" . key "User-Agent")
  -- test the various lenses
  assertEqual "GET succeeds" status200 (r ^. responseStatus)
  assertEqual "GET succeeds 200" 200 (r ^. responseStatus . statusCode)
  assertEqual "GET succeeds OK" "OK" (r ^. responseStatus . statusMessage)
  assertEqual "GET response has HTTP/1.1 version" http11 (r ^. responseVersion)
  assertBool "GET response has Content-Type header" $
    isJust (r ^? responseHeader "Content-Type")
  assertBool "GET response has Date header" $
    isJust (lookup "Date" <$> r ^? responseHeaders)

basicPost site = do
  r <- post (site "/post") ("wibble" :: ByteString) >>= asValue
  let body = r ^. responseBody
  assertEqual "POST succeeds" status200 (r ^. responseStatus)
  assertEqual "POST echoes input" (Just "wibble") (body ^? key "data")
  assertEqual "POST is binary" (Just "application/octet-stream")
                               (body ^? key "headers" . key "Content-Type")

multipartPost site =
  withSystemTempFile "foo.html" $ \name handle -> do
    hPutStr handle "<!DOCTYPE html><html></html"
    hClose handle
    r <- post (site "/post") (partFile "html" name)
    assertEqual "POST succeeds" status200 (r ^. responseStatus)

basicHead site = do
  r <- head_ (site "/get")
  assertEqual "HEAD succeeds" status200 (r ^. responseStatus)

basicPut site = do
  r <- put (site "/put") ("wibble" :: ByteString)
  assertEqual "PUT succeeds" status200 (r ^. responseStatus)

basicDelete site = do
  r <- delete (site "/delete")
  assertEqual "DELETE succeeds" status200 (r ^. responseStatus)

throwsStatusCode site =
    assertThrows "404 causes exception to be thrown" inspect $
    head_ (site "/status/404")
  where inspect e = case e of
                      StatusCodeException _ _ _ -> return ()
                      _ -> assertFailure "unexpected exception thrown"

getBasicAuth site = do
  let opts = defaults & auth ?~ basicAuth "user" "passwd"
  r <- getWith opts (site "/basic-auth/user/passwd")
  assertEqual "basic auth GET succeeds" status200 (r ^. responseStatus)
  let inspect e = case e of
                    StatusCodeException status _ _ ->
                      assertEqual "failed basic auth failed GET gives 401"
                        status401 status
  assertThrows "basic auth GET fails if password is bad" inspect $
    getWith opts (site "/basic-auth/user/asswd")

getRedirect site = do
  r <- get (site "/redirect/3")
  let stripProto = T.dropWhile (/=':')
      smap f (String s) = String (f s)
  assertEqual "redirect goes to /get"
    (Just . String . stripProto . T.pack . site $ "/get")
    (smap stripProto <$> (r ^. responseBody ^? key "url"))

getParams site = do
  let opts1 = defaults & param "foo" .~ ["bar"]
  r1 <- getWith opts1 (site "/get")
  assertEqual "params set correctly 1" (Just (object [("foo","bar")]))
    (r1 ^. responseBody ^? key "args")
  let opts2 = defaults & params .~ [("quux","baz")]
  r2 <- getWith opts2 (site "/get")
  assertEqual "params set correctly 2" (Just (object [("quux","baz")]))
    (r2 ^. responseBody ^? key "args")
  r3 <- getWith opts2 (site "/get?whee=wat")
  assertEqual "correctly handle mix of params from URI and Options"
    (Just (object [("quux","baz"),("whee","wat")]))
    (r3 ^. responseBody ^? key "args")

getHeaders site = do
  let opts = defaults & header "X-Wibble" .~ ["bar"]
  r <- getWith opts (site "/get")
  assertEqual "extra header set correctly"
    (Just "bar")
    (r ^. responseBody ^? key "headers" . key "X-Wibble")

getGzip site = do
  r <- get (site "/gzip")
  assertEqual "gzip decoded for us" (Just (Bool True))
    (r ^. responseBody ^? key "gzipped")

headRedirect site =
  assertThrows "HEAD of redirect throws exception" inspect $
    head_ (site "/redirect/3")
  where inspect e = case e of
                      StatusCodeException status _ _ ->
                        let code = status ^. statusCode
                        in assertBool "code is redirect"
                           (code >= 300 && code < 400)

redirectOverflow site =
  assertThrows "GET with too many redirects throws exception" inspect $
    getWith (defaults & redirects .~ 3) (site "/redirect/5")
  where inspect e = case e of TooManyRedirects _ -> return ()

invalidURL _site = do
  let noProto (InvalidUrlException _ _) = return ()
  assertThrows "exception if no protocol" noProto (get "wheeee")
  let noHost (InvalidDestinationHost _) = return ()
  assertThrows "exception if no host" noHost (get "http://")

funkyScheme site = do
  -- schemes are case insensitive, per RFC 3986 section 3.1
  let (scheme, rest) = break (==':') $ site "/get"
  void . get $ map toUpper scheme <> rest

cookiesSet site = do
  r <- get (site "/cookies/set?x=y")
  assertEqual "cookies are set correctly" (Just "y")
    (r ^? responseCookie "x" . cookieValue)

cookieSession site = Session.withSession $ \s -> do
  void $ Session.get s (site "/cookies/set?foo=bar")
  r <- Session.get s (site "/cookies")
  assertEqual "cookies are set correctly" (Just "bar")
    (r ^? responseCookie "foo" . cookieValue)
  assertEqual "whee" (Just "bar")
    (r ^. responseBody ^? key "cookies" . key "foo")

getWithManager site = withManager $ \opts -> do
  void $ getWith opts (site "/get?a=b")
  void $ getWith opts (site "/get?b=c")

assertThrows :: (Show e, Exception e) => String -> (e -> IO ()) -> IO a -> IO ()
assertThrows desc inspect act = do
  let myInspect e = inspect e `E.catch` \(ee :: E.PatternMatchFail) ->
        assertFailure (desc <> ": unexpected exception (" <>
                       show e <> "): " <> show ee)
  caught <- (act >> return False) `E.catch` \e -> myInspect e >> return True
  unless caught (assertFailure desc)

commonTestsWith site = [
    testGroup "basic" [
      testCase "get" $ basicGet site
    , testCase "post" $ basicPost site
    , testCase "head" $ basicHead site
    , testCase "put" $ basicPut site
    , testCase "delete" $ basicDelete site
    , testCase "404" $ throwsStatusCode site
    , testCase "headRedirect" $ headRedirect site
    , testCase "redirectOverflow" $ redirectOverflow site
    , testCase "invalidURL" $ invalidURL site
    , testCase "funkyScheme" $ funkyScheme site
    ]
  , testGroup "fancy" [
      testCase "basic auth" $ getBasicAuth site
    , testCase "redirect" $ getRedirect site
    , testCase "params" $ getParams site
    , testCase "headers" $ getHeaders site
    , testCase "gzip" $ getGzip site
    , testCase "getWithManager" $ getWithManager site
    ]
  ]

httpbinTestsWith site = commonTestsWith site <> [
    testCase "cookiesSet" $ cookiesSet site
  , testCase "cookieSession" $ cookieSession site
  ]

-- Tests that our local httpbin clone doesn't yet support.
httpbinTests = [testGroup "httpbin" [
    testGroup "http" $ httpbinTestsWith ("http://httpbin.org" <>)
  , testGroup "https" $ httpbinTestsWith ("https://httpbin.org" <>)
  ]]

startServer = do
  started <- newEmptyMVar
  let go n | n >= 100 = putMVar started Nothing
           | otherwise = do
        let port = 8000 + n
            startedUp p = putMVar started (Just ("http://localhost:" <> p))
            mkCfg = return . setBind ("localhost") . setPort port .
                    setVerbose False .
                    setStartupHook (const (startedUp (show port)))
        serve mkCfg `E.catch` \(_::E.IOException) -> go (n+1)
  tid <- forkIO $ go 0
  (,) tid <$> takeMVar started

main = do
  (tid, mserv) <- startServer
  flip E.finally (killThread tid) .
    defaultMain $ httpbinTests <>
    case mserv of
      Nothing -> []
      Just binding -> [testGroup "localhost" $ commonTestsWith (binding <>)]

localTests = commonTestsWith ("http://localhost:8000" <>)
