module Main (main) where

import Test.Framework (testGroup)
import UnitTests (testWith)
import qualified AWS (tests)
import qualified Properties.Store

main :: IO ()
main = do
  awsTests <- AWS.tests
  testWith [
      testGroup "store" Properties.Store.tests
    , awsTests
    ]
