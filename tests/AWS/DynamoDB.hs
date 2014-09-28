{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module AWS.DynamoDB (tests) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Aeson.Lens (key, _String, values, _Double)
import Data.Aeson.QQ
import Data.Text as T (pack)
import Network.Wreq
import Network.Wreq.Lens (statusMessage)
import System.Timeout (timeout)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertFailure)

-- FIXME: retry create call in case the table is in DELETING state
-- from a previous test run (error 'Table already exists: ...'). For
-- now 'create' testcase and all others will fails. Rerun when ongoing
-- delete operation is complete.

tests :: String -> String -> Options -> Test
tests prefix region baseopts = testGroup "dynamodb" [
    testCase "createTable" $ createTable prefix region baseopts
  , testCase "listTables" $ listTables prefix region baseopts
  , testCase "awaitTableActive"  $ awaitTableActive prefix region baseopts
  , testCase "putItem" $ putItem prefix region baseopts
  , testCase "getItem" $ getItem prefix region baseopts
  , testCase "deleteItem" $ deleteItem prefix region baseopts
  , testCase "deleteTable" $ deleteTable prefix region baseopts -- call last
  ]

createTable :: String -> String -> Options -> IO ()
createTable prefix region baseopts = do
  let opts = baseopts
             & header "X-Amz-Target" .~ ["DynamoDB_20120810.CreateTable"]
             & header "Content-Type" .~ ["application/x-amz-json-1.0"]
  r <- postWith opts (url region) $
         [aesonQQ| {
             "TableName": #{prefix ++ tablename},
             "KeySchema": [
               { "AttributeName": "name", "KeyType": "HASH" },
               { "AttributeName": "age", "KeyType": "RANGE" }
             ],
             "AttributeDefinitions": [
               { "AttributeName": "name", "AttributeType": "S" },
               { "AttributeName": "age", "AttributeType": "S" }
             ],
             "ProvisionedThroughput": {
               "ReadCapacityUnits": 1,
               "WriteCapacityUnits": 1
             }
         } |]
  assertBool "createTables 200" $ r ^. responseStatus . statusCode == 200
  assertBool "createTables OK" $ r ^. responseStatus . statusMessage == "OK"
  assertBool "createTables status CREATING" $
    r ^. responseBody . key "TableDescription" . key "TableStatus" . _String == "CREATING"
  assertBool "createTables no items in new table" $
    r ^? responseBody . key "TableDescription" . key "ItemCount" . _Double == Just 0

listTables :: String -> String -> Options -> IO ()
listTables prefix region baseopts = do
  let opts = baseopts
             & header "X-Amz-Target" .~ ["DynamoDB_20120810.ListTables"]
             & header "Content-Type" .~ ["application/x-amz-json-1.0"]
  -- FIXME avoid limit to keep tests from failing if there are > tables?
  r <- postWith opts (url region) [aesonQQ| { "Limit": 100 } |]
  assertBool "listTables 200" $ r ^. responseStatus . statusCode == 200
  assertBool "listTables OK" $ r ^. responseStatus . statusMessage == "OK"
  assertBool "listTables contains test table" $
    elem (T.pack $ prefix ++ tablename)
         (r ^.. responseBody . key "TableNames" . values . _String)

awaitTableActive :: String -> String -> Options -> IO ()
awaitTableActive prefix region baseopts = do
  let dur = 45 -- typically ACTIVE in 20s or less (us-west-2, Sept 2014)
  res <- timeout (dur*1000*1000) check
  case res of
    Nothing ->
      assertFailure $ "timeout: table not ACTIVE after " ++ show dur ++ "s"
    Just () ->
      return () -- PASS
  where
    check = do
      let opts = baseopts
                 & header "X-Amz-Target" .~ ["DynamoDB_20120810.DescribeTable"]
                 & header "Content-Type" .~ ["application/x-amz-json-1.0"]
      r <- postWith opts (url region)
             [aesonQQ| { "TableName": #{prefix ++ tablename} } |]
      assertBool "awaitTableActive 200" $ r ^. responseStatus . statusCode == 200
      assertBool "awaitTableActive OK" $ r ^. responseStatus . statusMessage == "OK"
      -- Prelude.putStr "."
      case r ^. responseBody . key "Table" . key "TableStatus" . _String of
        "ACTIVE" ->
          return ()
        _ -> do
          threadDelay $ 5*1000*1000 -- 5 sleep
          check

deleteTable :: String -> String -> Options -> IO ()
deleteTable prefix region baseopts = do
  let opts = baseopts
             & header "X-Amz-Target" .~ ["DynamoDB_20120810.DeleteTable"]
             & header "Content-Type" .~ ["application/x-amz-json-1.0"]
  r <- postWith opts (url region) $
         [aesonQQ| { "TableName": #{prefix ++ tablename} } |]
  assertBool "deleteTable 200" $ r ^. responseStatus . statusCode == 200
  assertBool "deleteTable OK" $ r ^. responseStatus . statusMessage == "OK"

putItem :: String -> String -> Options -> IO ()
putItem prefix region baseopts = do
  let opts = baseopts
             & header "X-Amz-Target" .~ ["DynamoDB_20120810.PutItem"]
             & header "Content-Type" .~ ["application/x-amz-json-1.0"]
  r <- postWith opts (url region) $
         [aesonQQ| {
           "TableName": #{prefix ++ tablename},
           "Item": {
             "name": { "S": "someone" },
             "age": {"S": "whatever"},
             "bar": {"S": "baz"}
           }
         } |]
  assertBool "putItem 200" $ r ^. responseStatus . statusCode == 200
  assertBool "putItem OK" $ r ^. responseStatus . statusMessage == "OK"

getItem :: String -> String -> Options -> IO ()
getItem prefix region baseopts = do
  let opts = baseopts
             & header "X-Amz-Target" .~ ["DynamoDB_20120810.GetItem"]
             & header "Content-Type" .~ ["application/x-amz-json-1.0"]
  r <- postWith opts (url region) $
         [aesonQQ| {
           "TableName": #{prefix ++ tablename},
           "Key": {
             "name": { "S": "someone" },
             "age": {"S": "whatever"}
           },
           "AttributesToGet": [ "bar" ],
           "ConsistentRead": true,
           "ReturnConsumedCapacity": "TOTAL"
         } |]
  assertBool "getItem 200" $ r ^. responseStatus . statusCode == 200
  assertBool "getItem OK" $ r ^. responseStatus . statusMessage == "OK"
  assertBool "getItem baz value is bar" $
    r ^. responseBody . key "Item" . key "bar" . key "S" . _String == "baz"

deleteItem :: String -> String -> Options -> IO ()
deleteItem prefix region baseopts = do
  let opts = baseopts
             & header "X-Amz-Target" .~ ["DynamoDB_20120810.DeleteItem"]
             & header "Content-Type" .~ ["application/x-amz-json-1.0"]
  r <- postWith opts (url region) $
         [aesonQQ| {
           "TableName": #{prefix ++ tablename},
           "Key": {
             "name": { "S": "someone" },
             "age": {"S": "whatever"}
           },
           "ReturnValues": "ALL_OLD"
         } |]
  assertBool "getItem 200" $ r ^. responseStatus . statusCode == 200
  assertBool "getItem OK" $ r ^. responseStatus . statusMessage == "OK"

url :: String -> String
url region =
  "https://dynamodb." ++ region ++ ".amazonaws.com/"

tablename :: String
tablename =
  "test"
