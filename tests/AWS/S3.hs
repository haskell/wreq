{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module AWS.S3 (tests) where

import AWS.Aeson
import Control.Lens hiding ((.=))
import Data.Char (toLower)
import Data.Monoid ((<>))
import Network.Wreq
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.ByteString.Char8 as BS8 (ByteString, pack)

-- FIXME: retry create call in case we get the S3 specific "A
-- conflicting conditional operation is currently in progress against
-- this resource. Please try again." error from a previous test run
-- that is still deleting the test bucket. For now the 'create'
-- testcase and all others will fails.

tests :: String -> String -> Options -> Test
tests prefix region baseopts = let
  lowerPrefix = map toLower prefix
  in testGroup "s3" [
        testCase "createBucket" $ createBucket lowerPrefix region baseopts
      , testCase "putObjectJSON" $ putObjectJSON lowerPrefix region baseopts
      , testCase "getObjectJSON" $ getObjectJSON lowerPrefix region baseopts
      , testCase "deleteObjectJSON" $ deleteObjectJSON lowerPrefix region baseopts
      , testCase "deleteBucket" $ deleteBucket lowerPrefix region baseopts -- call last
      ]

createBucket :: String -> String -> Options -> IO ()
createBucket prefix region baseopts = do
  r <- putWith baseopts (url region ++ prefix ++ "testbucket") $
         locationConstraint region
  assertBool "createBucket 200" $ r ^. responseStatus . statusCode == 200
  assertBool "createBucket OK" $ r ^. responseStatus . statusMessage == "OK"

deleteBucket :: String -> String -> Options -> IO ()
deleteBucket prefix region baseopts = do
  r <- deleteWith baseopts (url region ++ prefix ++ "testbucket")
  assertBool "deleteBucket 204 - no content" $
    r ^. responseStatus . statusCode == 204
  assertBool "deleteBucket OK" $
    r ^. responseStatus . statusMessage == "No Content"

putObjectJSON :: String -> String -> Options -> IO ()
putObjectJSON prefix region baseopts = do
  -- S3 write object, incl. correct content-type, uses /bucket/object syntax
  r <- putWith baseopts (url region ++ prefix ++ "testbucket/blabla-json") $
       object ["test" .= "key", "testdata" .= [1, 2, 3]]
  assertBool "putObjectJSON 200" $ r ^. responseStatus . statusCode == 200
  assertBool "putObjectJSON OK" $ r ^. responseStatus . statusMessage == "OK"

getObjectJSON :: String -> String -> Options -> IO ()
getObjectJSON prefix region baseopts = do
  r <- getWith baseopts (url region ++ prefix ++ "testbucket/blabla-json")
  assertBool "getObjectJSON 200" $ r ^. responseStatus . statusCode == 200
  assertBool "getObjectJSON OK" $ r ^. responseStatus . statusMessage == "OK"

deleteObjectJSON :: String -> String -> Options -> IO ()
deleteObjectJSON prefix region baseopts = do
  r <- deleteWith baseopts (url region ++ prefix ++ "testbucket/blabla-json")
  assertBool "deleteObjectJSON 204 - no content" $
    r ^. responseStatus . statusCode == 204
  assertBool "deleteObjectJSON OK" $
    r ^. responseStatus . statusMessage == "No Content"

-- see http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
url :: String -> String
url "us-east-1" = "https://s3.amazonaws.com/" -- uses 'classic'
url region      = "https://s3-" ++ region ++ ".amazonaws.com/"

-- see http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html
locationConstraint :: String -> BS8.ByteString
locationConstraint "us-east-1" = "" -- no loc needed for classic and Virginia
locationConstraint region = "<CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><LocationConstraint>" <> BS8.pack region <> "</LocationConstraint></CreateBucketConfiguration>"
