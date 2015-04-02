{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Framework (testGroup)
import UnitTests (testWith)
import qualified Properties.Store

#if defined(AWS_TESTS)
import qualified AWS (tests)
#endif

main :: IO ()
main = do
#if defined(AWS_TESTS)
  awsTests <- AWS.tests
#else
  let awsTests = testGroup "aws" []
#endif
  testWith [
      testGroup "store" Properties.Store.tests
    , awsTests
    ]
