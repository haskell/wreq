{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Lens ((^.), (^?), (.~), (&))
import Control.Monad (unless)
import Data.Aeson (Value(..), object)
import Data.Aeson.Lens (key)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Status (status200, status401)
import Network.HTTP.Types.Version (http11)
import Network.WReq
import Prelude hiding (head)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual, assertFailure)
import qualified Control.Exception as E

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
  r <- post (site "/post") (binary "wibble") >>= jsonValue
  let body = r ^. responseBody
  assertEqual "POST succeeds" status200 (r ^. responseStatus)
  assertEqual "POST echoes input" (Just "wibble") (body ^? key "data")
  assertEqual "POST is binary" (Just "application/octet-stream")
                               (body ^? key "headers" . key "Content-Type")

basicHead site = do
  r <- head (site "/get")
  assertEqual "HEAD succeeds" status200 (r ^. responseStatus)

basicPut site = do
  r <- put (site "/put") (binary "wibble")
  assertEqual "PUT succeeds" status200 (r ^. responseStatus)

basicDelete site = do
  r <- delete (site "/delete")
  assertEqual "DELETE succeeds" status200 (r ^. responseStatus)

throwsStatusCode site =
    assertThrows "404 causes exception to be thrown" inspect $
    head (site "/status/404")
  where inspect e = case e of
                      StatusCodeException _ _ _ -> return ()
                      _ -> assertFailure "unexpected exception thrown"

getBasicAuth site = do
  let opts = defaults & auth .~ basicAuth "user" "passwd"
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
  assertEqual "redirect goes to /get" (Just "http://httpbin.org/get")
    (r ^. responseBody ^? key "url")

getParams site = do
  let opts1 = defaults & param "foo" .~ ["bar"]
  r1 <- getWith opts1 (site "/get")
  assertEqual "params set correctly 1" (Just (object [("foo","bar")]))
    (r1 ^. responseBody ^? key "args")
  let opts2 = defaults & params .~ [("quux","baz")]
  r2 <- getWith opts2 (site "/get")
  assertEqual "params set correctly 2" (Just (object [("quux","baz")]))
    (r2 ^. responseBody ^? key "args")

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
    head (site "/redirect/3")
  where inspect e = case e of
                      StatusCodeException status _ _ ->
                        let code = status ^. statusCode
                        in assertBool "code is redirect"
                           (code >= 300 && code < 400)

redirectOverflow site =
  assertThrows "GET with too many redirects throws exception" inspect $
    getWith (defaults & redirects .~ 3) (site "/redirect/5")
  where inspect e = case e of TooManyRedirects _ -> return ()

invalidURL = do
  let noProto (InvalidUrlException _ _) = return ()
  assertThrows "exception if no protocol" noProto (get "wheeee")
  let noHost (InvalidDestinationHost _) = return ()
  assertThrows "exception if no host" noHost (get "http://")

assertThrows :: (Show e, Exception e) => String -> (e -> IO ()) -> IO a -> IO ()
assertThrows desc inspect act = do
  let myInspect e = inspect e `E.catch` \(ee :: E.PatternMatchFail) ->
        assertFailure (desc <> ": unexpected exception (" <>
                       show e <> "): " <> show ee)
  caught <- (act >> return False) `E.catch` \e -> myInspect e >> return True
  unless caught (assertFailure desc)

testsWith site = [
    testGroup "basic" [
      testCase "get" $ basicGet site
    , testCase "post" $ basicPost site
    , testCase "head" $ basicHead site
    , testCase "put" $ basicPut site
    , testCase "delete" $ basicDelete site
    , testCase "404" $ throwsStatusCode site
    , testCase "headRedirect" $ headRedirect site
    , testCase "redirectOverflow" $ redirectOverflow site
    , testCase "invalidURL" $ invalidURL
    ]
  , testGroup "fancy" [
      testCase "basic auth" $ getBasicAuth site
    , testCase "redirect" $ getRedirect site
    , testCase "params" $ getParams site
    , testCase "headers" $ getHeaders site
    , testCase "gzip" $ getGzip site
    ]
  ]

tests = [
    testGroup "http" $ testsWith ("http://httpbin.org" <>)
  , testGroup "https" $ testsWith ("https://httpbin.org" <>)
  ]

main = defaultMain tests
