{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Exception (Exception)
import Control.Lens ((^.), (^?))
import Control.Monad (unless)
import Data.Aeson (Value(..))
import Data.Aeson.Lens (key)
import Data.Maybe (isJust)
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Status (status200)
import Network.WReq
import Prelude hiding (head)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)
import qualified Control.Exception as E

basicGet = do
  r <- get "http://httpbin.org/get" >>= json
  assertEqual "GET succeeds" status200 (r ^. responseStatus)
  let body = r ^. responseBody :: Value
  assertBool "GET request has User-Agent header" $
    isJust (body ^? key "headers" . key "User-Agent")

basicPost = do
  r <- post "http://httpbin.org/post" (binary "wibble") >>= json
  let body = r^.responseBody :: Value
  assertEqual "POST succeeds" status200 (r ^. responseStatus)
  assertEqual "POST echoes input" (Just "wibble") (body ^? key "data")
  assertEqual "POST is binary" (Just "application/octet-stream")
                               (body ^? key "headers" . key "Content-Type")

basicHead = do
  r <- head "http://httpbin.org/get"
  assertEqual "HEAD succeeds" status200 (r ^. responseStatus)

basicPut = do
  r <- put "http://httpbin.org/put" (binary "wibble")
  assertEqual "PUT succeeds" status200 (r ^. responseStatus)

basicDelete = do
  r <- delete "http://httpbin.org/delete"
  assertEqual "DELETE succeeds" status200 (r ^. responseStatus)

throwsStatusCode = assertThrows "404 causes exception to be thrown" inspect $
                   head "http://httpbin.org/status/404"
  where inspect e = case e of
                      StatusCodeException _ _ _ -> return ()
                      _ -> assertFailure "unexpected exception thrown"

assertThrows :: Exception e => String -> (e -> IO ()) -> IO a -> IO ()
assertThrows desc inspect act = do
  caught <- (act >> return False) `E.catch` \e -> inspect e >> return True
  unless caught (assertFailure desc)

tests = [
    testGroup "basic" [
      testCase "get" basicGet
    , testCase "post" basicPost
    , testCase "head" basicHead
    , testCase "put" basicPut
    , testCase "delete" basicDelete
    , testCase "404" throwsStatusCode
    ]
  ]

main = defaultMain tests
