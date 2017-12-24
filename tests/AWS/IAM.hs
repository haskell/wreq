{-# LANGUAGE OverloadedLists, OverloadedStrings, DeriveGeneric #-}
module AWS.IAM (tests) where

import AWS.Aeson
import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Data.Aeson (encode)
import Data.Aeson.Lens (key, _String, values, _Value)
import Data.Char (toUpper)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text as T (Text, pack, unpack, split)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy as LT (toStrict)
import Data.Text.Lazy.Encoding as E (decodeUtf8)
import GHC.Generics
import Network.Wreq
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as DAT

tests :: String -> String -> Options -> IORef String -> Test
tests prefix region baseopts iamTestState = testGroup "iam" [
    testCase "listUsers"     $ listUsers prefix region baseopts
  , testCase "createRole"    $ createRole prefix region baseopts iamTestState
  , testCase "listRoles"     $ listRoles prefix region baseopts
  , testCase "putRolePolicy" $ putRolePolicy prefix region baseopts
  , testCase "stsAssumeRole" $ stsAssumeRole prefix region baseopts iamTestState
  , testCase "deleteRolePolicy" $ deleteRolePolicy prefix region baseopts
  , testCase "deleteRole"    $ deleteRole prefix region baseopts
  ]

listUsers :: String -> String -> Options -> IO ()
listUsers _prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["ListUsers"]
             & param  "Version" .~ ["2010-05-08"]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (iamUrl region)
  assertBool "listUsers 200" $ r ^. responseStatus . statusCode == 200
  assertBool "listUsers OK" $ r ^. responseStatus . statusMessage == "OK"

createRole :: String -> String -> Options -> IORef String -> IO ()
createRole prefix region baseopts iamTestState = do
  let opts = baseopts
             & param  "Action"  .~ ["CreateRole"]
             & param  "Version" .~ ["2010-05-08"]
             & param  "RoleName" .~ [T.pack $ prefix ++ roleName]
             & param  "AssumeRolePolicyDocument" .~ [rolePolicyDoc]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (iamUrl region)
  assertBool "createRole 200" $ r ^. responseStatus . statusCode == 200
  assertBool "createRole OK" $ r ^. responseStatus . statusMessage == "OK"
  let [arn] = r ^.. responseBody . key "CreateRoleResponse"
                                 . key "CreateRoleResult"
                                 . key "Role"
                                 . key "Arn" . _String
  writeIORef iamTestState $ T.unpack arn

putRolePolicy :: String -> String -> Options -> IO ()
putRolePolicy prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["PutRolePolicy"]
             & param  "Version" .~ ["2010-05-08"]
             & param  "RoleName" .~ [T.pack $ prefix ++ roleName]
             & param  "PolicyName" .~ [testPolicyName]
             & param  "PolicyDocument" .~ [policyDoc]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (iamUrl region)
  assertBool "putRolePolicy 200" $ r ^. responseStatus . statusCode == 200
  assertBool "putRolePolicy OK" $ r ^. responseStatus . statusMessage == "OK"
  threadDelay $ 30*1000*1000 -- 30 sleep, allow change to propagate to region

deleteRolePolicy :: String -> String -> Options -> IO ()
deleteRolePolicy prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["DeleteRolePolicy"]
             & param  "Version" .~ ["2010-05-08"]
             & param  "RoleName" .~ [T.pack $ prefix ++ roleName]
             & param  "PolicyName" .~ [testPolicyName]
             & param  "PolicyDocument" .~ [policyDoc]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (iamUrl region)
  assertBool "deleteRolePolicy 200" $ r ^. responseStatus . statusCode == 200
  assertBool "deleteRolePolicy OK" $ r ^. responseStatus . statusMessage == "OK"

deleteRole :: String -> String -> Options -> IO ()
deleteRole prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["DeleteRole"]
             & param  "Version" .~ ["2010-05-08"]
             & param  "RoleName" .~ [T.pack $ prefix ++ roleName]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (iamUrl region)
  assertBool "deleteRole 200" $ r ^. responseStatus . statusCode == 200
  assertBool "deleteRole OK" $ r ^. responseStatus . statusMessage == "OK"

listRoles :: String -> String -> Options -> IO ()
listRoles prefix region baseopts = do
  let opts = baseopts
             & param  "Action"  .~ ["ListRoles"]
             & param  "Version" .~ ["2010-05-08"]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (iamUrl region)
  assertBool "listRoles 200" $ r ^. responseStatus . statusCode == 200
  assertBool "listRoles OK" $ r ^. responseStatus . statusMessage == "OK"
  let arns = r ^.. responseBody . key "ListRolesResponse" .
                                  key "ListRolesResult" .
                                  key "Roles" .
                                  values .
                                  key "Arn" . _String
  -- arns are of form: "arn:aws:iam::<acct>:role/ec2-role"
  let arns' = map (T.unpack . last . T.split (=='/')) arns
  assertBool "listRoles contains test role" $
    elem (prefix ++ roleName) arns'

-- Security Token Service (STS)
data Cred = Cred {
    accessKeyId :: T.Text,
    secretAccessKey :: T.Text,
    sessionToken :: T.Text,
    expiration :: Int -- Unix epoch
  } deriving (Generic, Show, Eq)

instance A.FromJSON Cred where
  parseJSON = DAT.genericParseJSON $ DAT.defaultOptions {
      DAT.fieldLabelModifier = \(h:t) -> toUpper h:t
    }

stsAssumeRole :: String -> String -> Options -> IORef String -> IO ()
stsAssumeRole prefix region baseopts iamTestState = do
  arn <- readIORef iamTestState
  let opts = baseopts
             & param  "Action"  .~ ["AssumeRole"]
             & param  "Version" .~ ["2011-06-15"]
             & param  "RoleArn" .~ [T.pack arn]
             & param  "ExternalId" .~ [externalId]
             & param  "RoleSessionName" .~ ["Bob"]
             & header "Accept"  .~ ["application/json"]
  r <- getWith opts (stsUrl region) -- STS call (part of IAM service family)
  let v = r ^? responseBody
            . key "AssumeRoleResponse"
            . key "AssumeRoleResult"
            . key "Credentials"
            . _Value
  assertBool "stsAssumeRole 200" $ r ^. responseStatus . statusCode == 200
  assertBool "stsAssumeRole OK" $ r ^. responseStatus . statusMessage == "OK"

  -- Now, use the temporary credentials to call an AWS service
  let cred = conv v :: Cred
  let key' = encodeUtf8 $ accessKeyId cred
  let secret' = encodeUtf8 $ secretAccessKey cred
  let token' = encodeUtf8 $ sessionToken cred
  let baseopts2 = defaults
                  & auth ?~ awsSessionTokenAuth AWSv4 key' secret' token'
  let opts2 = baseopts2
              & param  "Action"  .~ ["ListRoles"]
              & param  "Version" .~ ["2010-05-08"]
              & header "Accept"  .~ ["application/json"]
  r2 <- getWith opts2 (iamUrl region)
  assertBool "listRoles 200" $ r2 ^. responseStatus . statusCode == 200
  assertBool "listRoles OK" $ r2 ^. responseStatus . statusMessage == "OK"
  let arns = r2 ^.. responseBody . key "ListRolesResponse" .
                                  key "ListRolesResult" .
                                  key "Roles" .
                                  values .
                                  key "Arn" . _String
  -- arns are of form: "arn:aws:iam::<acct>:role/ec2-role"
  let arns' = map (T.unpack . last . T.split (=='/')) arns
  assertBool "listRoles contains test role" $
    elem (prefix ++ roleName) arns'

  where
    conv :: DAT.FromJSON a => Maybe DAT.Value -> a
    conv v = case v of
      Nothing -> error "1"
      Just x ->
        case A.fromJSON x of
          A.Success r ->
            r
          A.Error e ->
            error $ show e

iamUrl :: String -> String
iamUrl _ =
  "https://iam.amazonaws.com/" -- IAM is not region specific

stsUrl :: String -> String
stsUrl _region =
  "https://sts.amazonaws.com/" -- keep from needing to enable STS in regions
  -- To test region specific behavior, uncomment the line below
  --   "https://sts." ++ _region ++ ".amazonaws.com/" -- region specific
  -- Note: to access AWS STS in any region other than us-east-1, or the default
  --   region (sts.amazonaws.com), STS needs to be enabled in the
  --   AWS Management Console under
  --   Account Settings > Security Token Service Region
  --   If you forget, the AssumeRole call will return a 403 error with:
  --     "STS is not activated in this region for account:<acct>.
  --      Your account administrator can activate STS in this region using
  --      the IAM Console."

roleName :: String
roleName = "test"

testPolicyName :: T.Text
testPolicyName = "testPolicy"

-- Note that ExternalId is a concept used for cross account use cases
-- with 3rd parties. But the check works for same-account as well, which
-- makes it more convenient to test.
-- For more, see:
-- http://docs.aws.amazon.com/STS/latest/UsingSTS/sts-delegating-externalid.html
externalId :: T.Text
externalId = "someExternalId"

rolePolicyDoc :: T.Text
rolePolicyDoc = LT.toStrict . E.decodeUtf8 . encode $
  object [
      "Version" .= "2012-10-17",
      "Statement" .= [
        object [
          "Effect" .= "Allow",
          "Action" .= "sts:AssumeRole",
          "Principal" .= object ["AWS" .= "*"],
          "Condition" .= object ["StringEquals" .=
                                 object ["sts:ExternalId" .= string externalId]]
        ]
      ]
  ]

policyDoc :: T.Text
policyDoc = LT.toStrict . E.decodeUtf8 . encode $
  object [
      "Version" .= "2012-10-17",
      "Statement" .= [
        object [
          "Effect" .= "Allow",
          "Action" .= ["*"],
          "Resource" .= ["*"]
        ]
      ]
  ]