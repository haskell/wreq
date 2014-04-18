-- Examples of handling for JSON responses
--
-- This library provides several ways to handle JSON responses

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (.~))
import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Control.Exception as E

import Network.WReq


-- This Haskell type corresponds to the structure of a response body
-- from httpbin.org.

data GetBody = GetBody {
    headers :: Map Text Text
  , args :: Map Text Text
  , origin :: Text
  , url :: Text
  } deriving (Show, Generic)

-- Get GHC to derive a FromJSON instance for us.

instance FromJSON GetBody



-- We expect this to succeed.

basic_asJSON :: IO ()
basic_asJSON = do
  let opts = defaults & param "foo" .~ ["bar"]
  r <- getWith opts "http://httpbin.org/get" >>= asJSON
  putStrLn $ "args: " ++ show (args (r ^. responseBody))



-- The response we expect here is valid JSON, but cannot be converted
-- to an [Int], so this will throw a JSONError.

failing_asJSON :: IO ()
failing_asJSON = do
  (r :: Response [Int]) <- get "http://httpbin.org/get" >>= asJSON
  putStrLn $ "response: " ++ show (r ^. responseBody)



-- This demonstrates how to catch a JSONError.

failing_asJSON_catch :: IO ()
failing_asJSON_catch =
  failing_asJSON `E.catch` \(e :: JSONError) -> print e



-- Because asJSON is parameterized over MonadThrow, we can use it with
-- other instances.
--
-- Here, instead of throwing an exception in the IO monad, we instead
-- evaluate the result as an Either:
--
-- * if the conversion fails, the Left constructor will contain
--   whatever exception describes the error
--
-- * if the conversion succeeds, the Right constructor will contain
--   the converted response

either_asJSON :: IO ()
either_asJSON = do
  r <- get "http://httpbin.org/get"

  -- This first conversion attempt will fail, but because we're using
  -- Either, it will not throw an exception that kills execution.
  let failing = asJSON r :: Either E.SomeException (Response [Int])
  print failing

  -- Our second conversion attempt will succeed.
  let succeeding = asJSON r :: Either E.SomeException (Response GetBody)
  print (succeeding ^. responseBody)



main = do
  basic_asJSON
  failing_asJSON_catch
  either_asJSON
