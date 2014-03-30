{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Lens ((^.), (^?))
import Control.Monad (unless)
import Data.Aeson (Value(..))
import Data.Aeson.Lens (key)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Version (http11)
import Network.WReq
import Prelude hiding (head)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual, assertFailure)
import qualified Control.Exception as E

basicGet site = do
  r <- get (site "/get") >>= json
  let body = r ^. responseBody :: Value
  assertBool "GET request has User-Agent header" $
    isJust (body ^? key "headers" . key "User-Agent")
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
  r <- post (site "/post") (binary "wibble") >>= json
  let body = r^.responseBody :: Value
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

assertThrows :: Exception e => String -> (e -> IO ()) -> IO a -> IO ()
assertThrows desc inspect act = do
  caught <- (act >> return False) `E.catch` \e -> inspect e >> return True
  unless caught (assertFailure desc)

testsWith site = [
    testGroup "basic" [
      testCase "get" $ basicGet site
    , testCase "post" $ basicPost site
    , testCase "head" $ basicHead site
    , testCase "put" $ basicPut site
    , testCase "delete" $ basicDelete site
    , testCase "404" $ throwsStatusCode site
    ]
  ]

tests = [
    testGroup "http" $ testsWith ("http://httpbin.org" <>)
  , testGroup "https" $ testsWith ("https://httpbin.org" <>)
  ]

main = defaultMain tests
