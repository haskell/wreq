{-# LANGUAGE OverloadedStrings #-}
module AWS.SQS (tests) where

import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text as T (Text, pack, unpack, split)
import Lens.Micro
import Lens.Micro.Aeson (key, _String, values)
import Network.Wreq
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

-- FIXME: retry create call in case we get the SQS specific "wait 1
-- min after delete" error from a previous test run. For now the
-- 'create' testcase and all others will fails. Rerun after awaiting
-- the SQS 1 min window.

tests :: String -> String -> Options -> IORef String -> Test
tests prefix region baseopts sqsTestState = testGroup "sqs" [
    testCase "createQueue" $ createQueue prefix region baseopts sqsTestState
  , testCase "listQueues"  $ listQueues prefix region baseopts
  , testCase "sendMessage" $ sendMessage prefix region baseopts sqsTestState
  , testCase "receiveMessage" $ receiveMessage prefix region baseopts sqsTestState
  , testCase "deleteQueue" $ deleteQueue prefix region baseopts sqsTestState
  ]

createQueue :: String -> String -> Options -> IORef String -> IO ()
createQueue prefix region baseopts sqsTestState = do
  let opts = baseopts
             & param  "Action" .~ ["CreateQueue"]
             & param  "QueueName" .~ [T.pack $ prefix ++ queuename]
             & param  "Version" .~ ["2009-02-01"]
             & header "Accept" .~ ["application/json"]
  r <- getWith opts (url region)
  assertBool "listQueues 200" $ r ^. responseStatus . statusCode == 200
  assertBool "listQueues OK" $ r ^. responseStatus . statusMessage == "OK"
  let qurl = r ^. responseBody . key "CreateQueueResponse"
                              . key "CreateQueueResult"
                              . key "QueueUrl"
                              . _String
  writeIORef sqsTestState $ acctFromQueueUrl qurl

listQueues :: String -> String -> Options -> IO ()
listQueues prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["ListQueues"]
             & param  "Version" .~ ["2009-02-01"]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (url region)
  assertBool "listQueues 200" $ r ^. responseStatus . statusCode == 200
  assertBool "listQueues OK" $ r ^. responseStatus . statusMessage == "OK"
  let qurls = r ^.. responseBody . key "ListQueuesResponse" .
                                   key "ListQueuesResult" .
                                   key "queueUrls" .
                                   values . _String
  -- url of form: https://sqs.<region>.amazon.com/<acct>/<queuename>
  let qurls' = map (T.unpack . last . T.split (=='/')) qurls
  assertBool "listQueues contains test queue" $
    elem (prefix ++ queuename) qurls'

deleteQueue :: String -> String -> Options -> IORef String -> IO ()
deleteQueue prefix region baseopts sqsTestState = do
  acct <- readIORef sqsTestState
  let opts = baseopts
             & param "Action" .~ ["DeleteQueue"]
             & param "Version" .~ ["2009-02-01"]
             & header "Accept" .~ ["application/json"]
  r <- getWith opts (url region ++ acct ++ "/" ++ prefix ++ queuename)
  assertBool "deleteQueues 200" $ r ^. responseStatus . statusCode == 200
  assertBool "deleteQueues OK" $ r ^. responseStatus . statusMessage == "OK"

sendMessage :: String -> String -> Options -> IORef String -> IO ()
sendMessage prefix region baseopts sqsTestState = do
  acct <- readIORef sqsTestState
  let opts = baseopts
             & param "Action" .~ ["SendMessage"]
             & param "Version" .~ ["2012-11-05"]
             & param "MessageBody" .~ ["uffda"]
             & header "Accept" .~ ["application/json"]
  r <- getWith opts (url region ++ acct ++ "/" ++ prefix ++ queuename)
  assertBool "sendMessage 200" $ r ^. responseStatus . statusCode == 200
  assertBool "sendMessage OK" $ r ^. responseStatus . statusMessage == "OK"

receiveMessage :: String -> String -> Options -> IORef String -> IO ()
receiveMessage prefix region baseopts sqsTestState = do
  acct <- readIORef sqsTestState
  let opts = baseopts
             & param "Action" .~ ["ReceiveMessage"]
             & param "Version" .~ ["2009-02-01"]
             & header "Accept" .~ ["application/json"]
  r <- getWith opts (url region ++ acct ++ "/" ++ prefix ++ queuename)
  let [msg] = map T.unpack $ r ^.. responseBody . -- we sent only 1 message
                key "ReceiveMessageResponse" .
                key "ReceiveMessageResult" .
                key "messages" .
                values .
                key "Body" .
                _String
  assertBool "receiveMessage 200" $ r ^. responseStatus . statusCode == 200
  assertBool "receiveMessage OK" $ r ^. responseStatus . statusMessage == "OK"
  assertBool "receiveMessage match content" $ msg == "uffda"

url :: String -> String
url region =
  "https://sqs." ++ region ++ ".amazonaws.com/"

queuename :: String
queuename =
  "test"

-- url of form: https://sqs.<region>.amazon.com/<acct>/<queuename>
acctFromQueueUrl :: T.Text -> String
acctFromQueueUrl qurl =
  case T.split (=='/') qurl of
    _:_:_:acct:_ ->
      T.unpack acct
    _ ->
      "dummy"
