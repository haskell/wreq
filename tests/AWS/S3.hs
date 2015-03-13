{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module AWS.S3 (tests) where

import Control.Lens
import Data.Aeson.QQ
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
  t = \(mkUrl, label) ->
    testGroup (region ++ "_" ++ label)  [
      testCase "createBucket" $
        createBucket mkUrl lowerPrefix region baseopts
    , testCase "putObjectJSON" $
        putObjectJSON mkUrl lowerPrefix region baseopts
    , testCase "getObjectJSON" $
        getObjectJSON mkUrl lowerPrefix region baseopts
    , testCase "deleteObjectJSON" $
        deleteObjectJSON mkUrl lowerPrefix region baseopts
    , testCase "deleteBucket" $
        deleteBucket mkUrl lowerPrefix region baseopts -- call last
    ]
  in testGroup "s3" $ map t [ (urlPath,  "bucket-in-path")
                            , (urlVHost, "bucket-in-vhost") ]

-- Path based bucket access
createBucket :: MkURL -> String -> String -> Options -> IO ()
createBucket url prefix region baseopts = do
  r <- putWith baseopts (url region prefix "testbucket") $
         locationConstraint region
  assertBool "createBucket 200" $ r ^. responseStatus . statusCode == 200
  assertBool "createBucket OK" $ r ^. responseStatus . statusMessage == "OK"

deleteBucket :: MkURL -> String -> String -> Options -> IO ()
deleteBucket url prefix region baseopts = do
  r <- deleteWith baseopts (url region prefix "testbucket")
  assertBool "deleteBucket 204 - no content" $
    r ^. responseStatus . statusCode == 204
  assertBool "deleteBucket OK" $
    r ^. responseStatus . statusMessage == "No Content"

putObjectJSON :: MkURL -> String -> String -> Options -> IO ()
putObjectJSON url prefix region baseopts = do
  -- S3 write object, incl. correct content-type
  r <- putWith baseopts (url region prefix "testbucket" ++ "blabla-json") $
         [aesonQQ| { "test": "key", "testdata": [ 1, 2, 3 ] } |]
  assertBool "putObjectJSON 200" $ r ^. responseStatus . statusCode == 200
  assertBool "putObjectJSON OK" $ r ^. responseStatus . statusMessage == "OK"

getObjectJSON :: MkURL -> String -> String -> Options -> IO ()
getObjectJSON url prefix region baseopts = do
  r <- getWith baseopts (url region prefix "testbucket" ++ "blabla-json")
  assertBool "getObjectJSON 200" $ r ^. responseStatus . statusCode == 200
  assertBool "getObjectJSON OK" $ r ^. responseStatus . statusMessage == "OK"

deleteObjectJSON :: MkURL -> String -> String -> Options -> IO ()
deleteObjectJSON url prefix region baseopts = do
  r <- deleteWith baseopts (url region prefix "testbucket" ++ "blabla-json")
  assertBool "deleteObjectJSON 204 - no content" $
    r ^. responseStatus . statusCode == 204
  assertBool "deleteObjectJSON OK" $
    r ^. responseStatus . statusMessage == "No Content"

type MkURL = String -> String -> String -> String --region prefix bucket

-- see http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
urlPath :: MkURL
urlPath "us-east-1" prefix bucketname =
  "https://s3.amazonaws.com/" ++ prefix ++ bucketname ++ "/"-- uses 'classic'
urlPath region prefix bucketname =
  "https://s3-" ++ region ++ ".amazonaws.com/" ++ prefix ++ bucketname ++ "/"

-- Generate a VirtualHost style URL
urlVHost :: MkURL
urlVHost "us-east-1" prefix bucketname =
  "https://" ++ prefix ++ bucketname ++ ".s3.amazonaws.com/"
urlVHost region prefix bucketname =
  "https://" ++ prefix ++ bucketname ++ ".s3-" ++ region ++ ".amazonaws.com/"

-- see http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUT.html
-- and http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
locationConstraint :: String -> BS8.ByteString
locationConstraint "us-east-1"  = "" -- no loc needed for classic and Virginia
locationConstraint "external-1" = "" -- no loc needed for Virginia
locationConstraint region = "<CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><LocationConstraint>" <> BS8.pack region <> "</LocationConstraint></CreateBucketConfiguration>"
