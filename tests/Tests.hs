module Main (main) where

import Test.Framework (testGroup)
import UnitTests (testWith)
import qualified Properties.Store

main :: IO ()
main = testWith [
    testGroup "store" Properties.Store.tests
  ]
