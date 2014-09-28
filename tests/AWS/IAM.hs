{-# LANGUAGE OverloadedStrings #-}
module AWS.IAM (tests) where

import Control.Lens
import Network.Wreq
import Network.Wreq.Lens (statusMessage)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

tests :: String -> String -> Options -> Test
tests prefix region baseopts = testGroup "iam" [
    testCase "listUsers"  $ listUsers prefix region baseopts
  ]

listUsers :: String -> String -> Options -> IO ()
listUsers _prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["ListUsers"]
             & param  "Version" .~ ["2010-05-08"]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (url region)
  assertBool "listUsers 200" $ r ^. responseStatus . statusCode == 200
  assertBool "listUsers OK" $ r ^. responseStatus . statusMessage == "OK"

url :: String -> String
url _ =
  "https://iam.amazonaws.com/" -- not region specific
