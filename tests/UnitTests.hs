{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-missing-signatures
    -fno-warn-unused-binds #-}

module UnitTests (testWith) where

import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, throwIO)
import Control.Lens ((^.), (^?), (.~), (?~), (&), iso, ix, Traversal')
import Control.Monad (unless, void)
import Data.Aeson hiding (Options)
import Data.Aeson.Lens (key, AsValue, _Object)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import HttpBin.Server (serve)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.HTTP.Types.Status (status200, status401)
import Network.HTTP.Types.Version (http11)
import Network.Wreq hiding
  (get, post, head_, put, options, delete,
   getWith, postWith, headWith, putWith, optionsWith, deleteWith)
import Network.Wreq.Lens
import Network.Wreq.Types (Postable, Putable)
import Snap.Http.Server.Config
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool, assertEqual, assertFailure)
import qualified Control.Exception as E
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Network.Wreq.Session as Session
import qualified Data.ByteString.Lazy as L
import qualified Network.Wreq as Wreq

data Verb = Verb {
    get :: String -> IO (Response L.ByteString)
  , getWith :: Options -> String -> IO (Response L.ByteString)
  , post :: forall a. Postable a => String -> a -> IO (Response L.ByteString)
  , postWith :: forall a. Postable a => Options -> String -> a
             -> IO (Response L.ByteString)
  , head_ :: String -> IO (Response ())
  , headWith :: Options -> String -> IO (Response ())
  , put :: forall a. Putable a => String -> a -> IO (Response L.ByteString)
  , putWith :: forall a. Putable a => Options -> String -> a -> IO (Response L.ByteString)
  , options :: String -> IO (Response ())
  , optionsWith :: Options -> String -> IO (Response ())
  , delete :: String -> IO (Response L.ByteString)
  , deleteWith :: Options -> String -> IO (Response L.ByteString)
  }

basic :: Verb
basic = Verb { get = Wreq.get, getWith = Wreq.getWith, post = Wreq.post
             , postWith = Wreq.postWith, head_ = Wreq.head_
             , headWith = Wreq.headWith, put = Wreq.put
             , putWith = Wreq.putWith, options = Wreq.options
             , optionsWith = Wreq.optionsWith, delete = Wreq.delete
             , deleteWith = Wreq.deleteWith }

session :: Session.Session -> Verb
session s = Verb { get = Session.get s
                 , getWith = flip Session.getWith s
                 , post = Session.post s
                 , postWith = flip Session.postWith s
                 , head_ = Session.head_ s
                 , headWith = flip Session.headWith s
                 , put = Session.put s
                 , putWith = flip Session.putWith s
                 , options = Session.options s
                 , optionsWith = flip Session.optionsWith s
                 , delete = Session.delete s
                 , deleteWith = flip Session.deleteWith s }

-- Helper aeson lens for case insensitive keys
-- The test 'snap' server unfortunately lowercases all headers, we have to be case-insensitive
-- when checking the returned header list.
cikey :: AsValue t => T.Text -> Traversal' t Value
cikey i = _Object . toInsensitive . ix (CI.mk i)
  where
    toInsensitive = iso toCi fromCi
    toCi = HMap.fromList . map (first CI.mk) . HMap.toList
    fromCi = HMap.fromList . map (first CI.original) . HMap.toList

basicGet Verb{..} site = do
  r <- get (site "/get")
  assertBool "GET request has User-Agent header" $
    isJust (r ^. responseBody ^? key "headers" . cikey "User-Agent")
  -- test the various lenses
  assertEqual "GET succeeds" status200 (r ^. responseStatus)
  assertEqual "GET succeeds 200" 200 (r ^. responseStatus . statusCode)
  assertEqual "GET succeeds OK" "OK" (r ^. responseStatus . statusMessage)
  assertEqual "GET response has HTTP/1.1 version" http11 (r ^. responseVersion)
  assertBool "GET response has Content-Type header" $
    isJust (r ^? responseHeader "Content-Type")
  assertBool "GET response has Date header" $
    isJust (lookup "Date" <$> r ^? responseHeaders)

basicPost Verb{..} site = do
  r <- post (site "/post") ("wibble" :: ByteString) >>= asValue
  let body = r ^. responseBody
  assertEqual "POST succeeds" status200 (r ^. responseStatus)
  assertEqual "POST echoes input" (Just "wibble") (body ^? key "data")
  assertEqual "POST is binary" (Just "application/octet-stream")
                               (body ^? key "headers" . cikey "Content-Type")

multipartPost Verb{..} site =
  withSystemTempFile "foo.html" $ \name handle -> do
    hPutStr handle "<!DOCTYPE html><html></html"
    hClose handle
    r <- post (site "/post") (partFile "html" name)
    assertEqual "POST succeeds" status200 (r ^. responseStatus)

basicHead Verb{..} site = do
  r <- head_ (site "/get")
  assertEqual "HEAD succeeds" status200 (r ^. responseStatus)

basicPut Verb{..} site = do
  r <- put (site "/put") ("wibble" :: ByteString)
  assertEqual "PUT succeeds" status200 (r ^. responseStatus)

data SolrAdd = SolrAdd
    { doc          :: String
    , boost        :: Float
    , overwrite    :: Bool
    , commitWithin :: Integer
    }

instance ToJSON SolrAdd where
    toJSON (SolrAdd doc boost overwrite commitWithin) =
        object
            [
                "add" .= object
                [ "doc" .= toJSON doc
                , "boost" .= boost
                , "overwrite" .= overwrite
                , "commitWithin" .= commitWithin
                ]
            ]

solrAdd :: SolrAdd
solrAdd = SolrAdd "wibble" 1.0 True 10000

jsonPut Verb{..} site = do
  r <- put (site "/put") $ toJSON solrAdd
  assertEqual "toJSON PUT request has correct Content-Type header"
    (Just "application/json")
    (r ^. responseBody ^? key "headers" . cikey "Content-Type")

byteStringPut Verb{..} site = do
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  r <- putWith opts (site "/put") $ encode solrAdd
  assertEqual "ByteString PUT request has correct Content-Type header"
    (Just "application/json")
    (r ^. responseBody ^? key "headers" . cikey "Content-Type")

basicDelete Verb{..} site = do
  r <- delete (site "/delete")
  assertEqual "DELETE succeeds" status200 (r ^. responseStatus)

throwsStatusCode Verb{..} site =
    assertThrows "404 causes exception to be thrown" inspect $
    head_ (site "/status/404")
  where inspect (HttpExceptionRequest _ e) = case e of
                      StatusCodeException _ _ -> return ()
                      _ -> assertFailure "unexpected exception thrown"
        inspect _ = assertFailure "unexpected exception thrown"

getBasicAuth Verb{..} site = do
  let opts = defaults & auth ?~ basicAuth "user" "passwd"
  r <- getWith opts (site "/basic-auth/user/passwd")
  assertEqual "basic auth GET succeeds" status200 (r ^. responseStatus)
  let inspect (HttpExceptionRequest _ e) = case e of
                    StatusCodeException resp _ ->
                      assertEqual "basic auth failed GET gives 401"
                        status401 (resp ^. responseStatus)
      inspect _ = assertFailure "unexpected exception thrown"

  assertThrows "basic auth GET fails if password is bad" inspect $
    getWith opts (site "/basic-auth/user/asswd")

getOAuth2 Verb{..} kind ctor site = do
  let opts = defaults & auth ?~ ctor "token1234"
  r <- getWith opts (site $ "/oauth2/" <> kind <> "/token1234")
  assertEqual ("oauth2 " <> kind <> " GET succeeds")
    status200 (r ^. responseStatus)
  let inspect (HttpExceptionRequest _ e) = case e of
                    StatusCodeException resp _ ->
                      assertEqual ("oauth2 " <> kind <> " failed GET gives 401")
                        status401 (resp ^. responseStatus)
      inspect _ = assertFailure "unexpected exception thrown"
  assertThrows ("oauth2 " <> kind <> " GET fails if token is bad") inspect $
    getWith opts (site $ "/oauth2/" <> kind <> "/token123")

getRedirect Verb{..} site = do
  r <- get (site "/redirect/3")
  let stripProto = T.dropWhile (/=':')
      smap f (String s) = String (f s)
  assertEqual "redirect goes to /get"
    (Just . String . stripProto . T.pack . site $ "/get")
    (smap stripProto <$> (r ^. responseBody ^? key "url"))

getParams Verb{..} site = do
  let opts1 = defaults & param "foo" .~ ["bar"]
  r1 <- getWith opts1 (site "/get")
  assertEqual "params set correctly 1" (Just (object [("foo","bar")]))
    (r1 ^. responseBody ^? key "args")
  let opts2 = defaults & params .~ [("quux","baz")]
  r2 <- getWith opts2 (site "/get")
  assertEqual "params set correctly 2" (Just (object [("quux","baz")]))
    (r2 ^. responseBody ^? key "args")
  r3 <- getWith opts2 (site "/get?whee=wat")
  assertEqual "correctly handle mix of params from URI and Options"
    (Just (object [("quux","baz"),("whee","wat")]))
    (r3 ^. responseBody ^? key "args")

getHeaders Verb{..} site = do
  let opts = defaults & header "X-Wibble" .~ ["bar"]
  r <- getWith opts (site "/get")
  assertEqual "extra header set correctly"
    (Just "bar")
    (r ^. responseBody ^? key "headers" . cikey "X-Wibble")

getCheckStatus Verb {..} site = do
  let opts = defaults & checkResponse .~ Just customRc
  r <- getWith opts (site "/status/404")
  assertThrows "Non 404 throws error" inspect $
    getWith opts (site "/get")
  assertEqual "Status 404"
    404
    (r ^. responseStatus . statusCode)
  where
    customRc :: ResponseChecker
    customRc _ resp
        | resp ^. responseStatus . statusCode == 404 = return ()
    customRc req resp = throwIO $ HttpExceptionRequest req (StatusCodeException (void resp) "")

    inspect (HttpExceptionRequest _ e) = case e of
        (StatusCodeException resp _) ->
            assertEqual "200 Status Error" (resp ^. responseStatus) status200
    inspect _ = assertFailure "unexpected exception thrown"


getGzip Verb{..} site = do
  r <- get (site "/gzip")
  assertEqual "gzip decoded for us" (Just (Bool True))
    (r ^. responseBody ^? key "gzipped")

headRedirect Verb{..} site = do
  assertThrows "HEAD of redirect throws exception" inspect $
    head_ (site "/redirect/3")
  where inspect (HttpExceptionRequest _ e) = case e of
                      StatusCodeException resp _ ->
                        let code = resp ^. responseStatus . statusCode
                        in assertBool "code is redirect"
                           (code >= 300 && code < 400)
        inspect _ = assertFailure "unexpected exception thrown"


redirectOverflow Verb{..} site =
  assertThrows "GET with too many redirects throws exception" inspect $
    getWith (defaults & redirects .~ 3) (site "/redirect/5")
  where inspect (HttpExceptionRequest _ e) = case e of TooManyRedirects _ -> return ()
        inspect _ = assertFailure "unexpected exception thrown"

invalidURL Verb{..} _site = do
  let noProto (InvalidUrlException _ _) = return ()
  assertThrows "exception if no protocol" noProto (get "wheeee")
  let noHost (HttpExceptionRequest _ (InvalidDestinationHost _)) = return ()
  assertThrows "exception if no host" noHost (get "http://")

funkyScheme Verb{..} site = do
  -- schemes are case insensitive, per RFC 3986 section 3.1
  let (scheme, rest) = break (==':') $ site "/get"
  void . get $ map toUpper scheme <> rest

cookiesSet Verb{..} site = do
  r <- get (site "/cookies/set?x=y")
  assertEqual "cookies are set correctly" (Just "y")
    (r ^? responseCookie "x" . cookieValue)


cookieSession site = Session.withSession $ \s -> do
  r0 <- Session.get s (site "/cookies/set?foo=bar")
  assertEqual "after set foo, foo set" (Just "bar")
    (r0 ^? responseCookie "foo" . cookieValue)
  assertEqual "a different accessor works" (Just "bar")
    (r0 ^. responseBody ^? key "cookies" . key "foo")

  r1 <- Session.get s (site "/cookies")
  assertEqual "long after set foo, foo still set" (Just "bar")
    (r1 ^? responseCookie "foo" . cookieValue)

  r2 <- Session.get s (site "/cookies/set?baz=quux")
  assertEqual "after set baz, foo still set" (Just "bar")
    (r2 ^? responseCookie "foo" . cookieValue)
  assertEqual "after set baz, baz set" (Just "quux")
    (r2 ^? responseCookie "baz" . cookieValue)

  r3 <- Session.get s (site "/cookies")
  assertEqual "long after set baz, foo still set" (Just "bar")
    (r3 ^? responseCookie "foo" . cookieValue)
  assertEqual "long after set baz, baz still set" (Just "quux")
    (r3 ^? responseCookie "baz" . cookieValue)

  r4 <- Session.get s (site "/cookies/delete?foo")
  assertEqual "after delete foo, foo deleted" Nothing
    (r4 ^? responseCookie "foo" . cookieValue)
  assertEqual "after delete foo, baz still set" (Just "quux")
    (r4 ^? responseCookie "baz" . cookieValue)

  r5 <- Session.get s (site "/cookies")
  assertEqual "long after delete foo, foo still deleted" Nothing
    (r5 ^? responseCookie "foo" . cookieValue)
  assertEqual "long after delete foo, baz still set" (Just "quux")
    (r5 ^? responseCookie "baz" . cookieValue)


getWithManager site = withManager $ \opts -> do
  void $ Wreq.getWith opts (site "/get?a=b")
  void $ Wreq.getWith opts (site "/get?b=c")

assertThrows :: (Show e, Exception e) => String -> (e -> IO ()) -> IO a -> IO ()
assertThrows desc inspect act = do
  let myInspect e = inspect e `E.catch` \(ee :: E.PatternMatchFail) ->
        assertFailure (desc <> ": unexpected exception (" <>
                       show e <> "): " <> show ee)
  caught <- (act >> return False) `E.catch` \e -> myInspect e >> return True
  unless caught (assertFailure desc)

commonTestsWith verb site = [
    testGroup "basic" [
      testCase "get" $ basicGet verb site
    , testCase "post" $ basicPost verb site
    , testCase "head" $ basicHead verb site
    , testCase "put" $ basicPut verb site
    , testCase "delete" $ basicDelete verb site
    , testCase "404" $ throwsStatusCode verb site
    , testCase "headRedirect" $ headRedirect verb site
    , testCase "redirectOverflow" $ redirectOverflow verb site
    , testCase "invalidURL" $ invalidURL verb site
    , testCase "funkyScheme" $ funkyScheme verb site
    ]
  , testGroup "fancy" [
      testCase "basic auth" $ getBasicAuth verb site
    , testCase "redirect" $ getRedirect verb site
    , testCase "params" $ getParams verb site
    , testCase "headers" $ getHeaders verb site
    , testCase "gzip" $ getGzip verb site
    , testCase "json put" $ jsonPut verb site
    , testCase "bytestring put" $ byteStringPut verb site
    , testCase "cookiesSet" $ cookiesSet verb site
    , testCase "getWithManager" $ getWithManager site
    , testCase "cookieSession" $ cookieSession site
    , testCase "getCheckStatus" $ getCheckStatus verb site
    ]
  ]

-- Snap responds incorrectly to HEAD (by sending a response body),
-- thereby killing http-client's ability to continue a session.
-- https://github.com/snapframework/snap-core/issues/192
snapHeadSessionBug site = Session.withSession $ \s -> do
  basicHead (session s) site
  -- will crash with (InvalidStatusLine "0")
  basicGet (session s) site

httpbinTestsWith verb site = commonTestsWith verb site <> [
  ]

-- Tests that our local httpbin clone doesn't yet support.
httpbinTests verb = [testGroup "httpbin" [
    testGroup "http" $ httpbinTestsWith verb ("http://httpbin.org" <>)
  , testGroup "https" $ httpbinTestsWith verb ("https://httpbin.org" <>)
  ]]

-- Tests that httpbin.org doesn't support.
localTests verb site = commonTestsWith verb site <> [
    testCase "oauth2 Bearer" $ getOAuth2 verb "Bearer" oauth2Bearer site
  , testCase "oauth2 token" $ getOAuth2 verb "token" oauth2Token site
  ]

startServer = do
  started <- newEmptyMVar
  let go n | n >= 100 = putMVar started Nothing
           | otherwise = do
        let port = 8000 + n
            startedUp p = putMVar started (Just ("http://0.0.0.0:" <> p))
            mkCfg = return . setBind ("0.0.0.0") . setPort port .
                    setVerbose False .
                    setStartupHook (const (startedUp (show port)))
        serve mkCfg `E.catch` \(_::E.IOException) -> go (n+1)
  tid <- forkIO $ go 0
  (,) tid <$> takeMVar started

testWith :: [Test] -> IO ()
testWith tests = do
  (tid, mserv) <- startServer
  Session.withSession $ \s ->
    flip E.finally (killThread tid) .
    defaultMain $ tests <>
                  [ testGroup "plain" $ httpbinTests basic
                  , testGroup "session" $ httpbinTests (session s)] <>
      case mserv of
        Nothing -> []
        Just binding -> [
            testGroup "localhost" [
              testGroup "plain" $ localTests basic (binding <>)
            , testGroup "session" $ localTests (session s) (binding <>)
            ]
          ]
