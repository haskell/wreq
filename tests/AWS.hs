{-# LANGUAGE ScopedTypeVariables #-}

{-
A set of end to end Amazon Web Services (AWS) tests to make sure we
can access a number of AWS services that use various AWS request
formats.

These tests help us guard against errors we may otherwise introduce
while refactoring or extending Wreq. The tests are not meant to
exercise the features of the respective AWS services exhaustively.

** ASSUMPTIONS **
To configure and run these tests you need an AWS account. We assume
that you are familiar with AWS concepts and the charging model.

** ENABLING AWS TESTS **
For now, enable AWS tests by setting the WREQ_AWS_ACCESS_KEY_ID
env variable per below.

TODO| To enable AWS tests use the `-faws` flag as part of
TODO|   $ cabal configure --enable-tests -faws ...
TODO| To capture code coverage information, add the `-fdeveloper` flag.

** REQUIRED CLIENT CONFIGURATION **
The tests require two environment variables:
   $ /bin/env WREQ_AWS_ACCESS_KEY_ID='...' \
              WREQ_AWS_SECRET_ACCESS_KEY='...' \
       cabal test

** CHARGES/COST **
These tests may incur small amounts of AWS charges for the minimum
DynamoDB IOs per second they provision and for the messages sent to
AWS SQS and objects stored in S3. These charges consume only a tiny
fraction of the AWS free tier allowance (if not used up otherwise).

** AWS REGIONS **
Tests are executed against the AWS Region `us-west-2` by default. You
can change the region by setting the AWS_REGION environment variable
(e.g. /bin/env WREQ_AWS_REGION=eu-west-1 cabal test).

In the case of S3, we translate 'us-east-1' to
's3-external-1.amazonaws.com' denoting the Virginia (only) endpoint.
(see http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region).

** AWS USER AND LEAST PRIVILEDGE POLICY **
The file `tests/AWS/policy.json` contains the least priviledge "AWS
Identity and Access (IAM)" policy sufficient to run these tests. It is
a best security practice to run the tests using an AWS IAM user you
created specifically for this purpose. Use the AWS IAM Management
Console to create a new user, get the WREQ_AWS_ACCESS_KEY and
WREQ_AWS_SECRET_KEY for that user and apply the policy to the user to
limit its priviledges.

**AVOID AWS RESOURCE NAME COLLISIONS IN CONCURRENT TESTS**
To run tests concurrently in same AWS account, set the environment
variable WREQ_AWS_TEST_PREFIX to a unique string for each test client
or machine. The default prefix used for all resources created
(e.g. DynamoDB tables, SQS queues, S3 buckets, etc.) is
`deleteWreqTest`.
-}

module AWS (tests) where

import Control.Exception as E (IOException, catch)
import Control.Lens
import Data.ByteString.Char8 as BS8 (pack)
import Data.IORef (newIORef)
import Network.Info (getNetworkInterfaces, mac, ipv6)
import Network.Wreq
import System.Environment (getEnv)
import Test.Framework (Test, testGroup)
import qualified AWS.DynamoDB (tests)
import qualified AWS.IAM (tests)
import qualified AWS.S3 (tests)
import qualified AWS.SQS (tests)

tests :: IO Test
tests = do
  -- TODO - use ... configure -faws ... in the future
  --        but couldn't figure out (yet) how to get
  --        a hold of the flag value in test code.
  -- Workaround: for now, the presence of the
  --   WREQ_AWS_ACCESS_KEY_ID
  -- env variable enables the tests.
  flag <- (getEnv "WREQ_AWS_ACCESS_KEY_ID" >> return True) `E.catch`
          \(_::IOException) -> return False
  tests0 flag

tests0 :: Bool -> IO Test
tests0 False =
  return $ testGroup "aws" [] -- skip AWS tests
tests0 True = do
  region <- env "us-west-2" "WREQ_AWS_REGION"
  key <- BS8.pack `fmap` getEnv "WREQ_AWS_ACCESS_KEY_ID"
  secret <- BS8.pack `fmap` getEnv "WREQ_AWS_SECRET_ACCESS_KEY"
  let baseopts = defaults & auth ?~ awsAuth AWSv4 key secret
  prefix <- env "deleteWreqTest" "WREQ_AWS_TEST_PREFIX"
  sqsTestState <- newIORef "missing"
  iamTestState <- newIORef "missing"
  uniq <- uniqueMachineId
  let bucketname = prefix ++ uniq
  return $ testGroup "aws" [
      AWS.DynamoDB.tests (prefix ++ "DynamoDB") region baseopts
    , AWS.IAM.tests (prefix ++ "IAM") region baseopts iamTestState
    , AWS.SQS.tests (prefix ++ "SQS") region baseopts sqsTestState
      -- S3 buckets are global entities and the namespace shared among
      -- all AWS customers. We will use a unique id based on the MAC
      -- address of our client to avoid naming conflicts among different
      -- developers running the tests.
    , AWS.S3.tests bucketname region baseopts
    --, AWS.S3.tests bucketname "us-east-1" baseopts -- classic
    --, AWS.S3.tests bucketname "external-1" baseopts -- Virginia
    ]

-- return a globally unique machine id (uses a MAC address)
uniqueMachineId :: IO String
uniqueMachineId = do
  nis <- getNetworkInterfaces
  let lmac = filter ((/=) "00:00:00:00:00:00") $ map (show . mac) nis
  -- travis-ci.org doesn't show mac addresses - use ipv6 (e.g. of venet0)
  let lipv6 = filter (\s -> (s /=    "0:0:0:0:0:0:0:0") &&
                            (s /=    "0:0:0:0:0:0:0:1") &&
                            (s /= "fe80:0:0:0:0:0:0:1"))
            $ map (show . ipv6) nis
  if (null $ lmac ++ lipv6)
    then error "FATAL: can't determine unique id automatically in this runtime env!"
    else do
      let uniq = concatMap (\c -> if c == ':' then [] else [c])
               $ head (lmac ++ lipv6)
      return uniq

env :: String -> String -> IO String
env defVal name = getEnv name `E.catch` \(_::IOException) -> return defVal
