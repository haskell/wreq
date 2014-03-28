{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Lens ((^.), (^?))
import Data.Aeson (Value(..))
import Data.Aeson.Lens (key)
import Data.Maybe (isJust)
import Network.HTTP.Types.Status (status200)
import Network.WReq
import Prelude hiding (head)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual)

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

tests = [
    testGroup "basic" [
      testCase "get" basicGet
    , testCase "post" basicPost
    , testCase "head" basicHead
    , testCase "put" basicPut
    , testCase "delete" basicDelete
    ]
  ]

main = defaultMain tests
