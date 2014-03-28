{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens ((^.), (^?))
import Data.Aeson (Value(..))
import Data.Aeson.Lens (key)
import Network.HTTP.Types.Status (status200)
import Network.WReq
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual)
import qualified Data.ByteString.Lazy as L

basicGet = do
  r <- get "http://httpbin.org/get"
  assertEqual "GET succeeds" status200 (r ^. responseStatus)
  assertBool "GET gives non-empty result" $ L.length (r^.responseBody) > 0

basicPost = do
  r <- post "http://httpbin.org/post" (binary "wibble") >>= json
  let body = r^.responseBody :: Value
  assertEqual "POST succeeds" status200 (r ^. responseStatus)
  assertEqual "POST echoes input" (Just "wibble") (body ^? key "data")
  assertEqual "POST is binary" (Just "application/octet-stream")
                               (body ^? key "headers" . key "Content-Type")

basicPut = do
  r <- put "http://httpbin.org/put" (binary "wibble")
  assertEqual "PUT succeeds" status200 (r ^. responseStatus)

tests = [
    testGroup "basic" [
      testCase "get" basicGet
    , testCase "post" basicPost
    , testCase "put" basicPut
    ]
  ]

main = defaultMain tests
