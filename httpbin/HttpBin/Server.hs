{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- TBD: basic-auth, gzip

module HttpBin.Server (serve) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), eitherDecode, object, toJSON)
import Data.Aeson.Encode.Pretty (Config(..), encodePretty')
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive (original)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Data.Time.Clock (UTCTime(..))
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Snap.Core
import Snap.Http.Server as Snap
import Snap.Util.GZip (withCompression)
import System.PosixCompat.Time (epochTime)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Encoding as Lazy

get = respond return

post = respond $ \obj -> do
  body <- readRequestBody 65536
  return $ obj <> [("data", toJSON (Lazy.decodeUtf8 body))] <>
           case eitherDecode body of
             Left _    -> [("json", Null)]
             Right val -> [("json", val)]

put = post

delete = respond return

patch = post

status = do
  val <- (fromMaybe 200 . rqIntParam "val") <$> getRequest
  let code | val >= 200 && val <= 505 = val
           | otherwise                = 400
  modifyResponse $ setResponseCode code

gzip =
  localRequest (setHeader "Accept-Encoding" "gzip") . withCompression .
  respond $ \obj -> return $ obj <> [("gzipped", Bool True)]

deleteCookies = do
  req <- getRequest
  let expire name = Cookie name "" (Just mcfly) Nothing (Just "/") False False
      mcfly = UTCTime (read "1985-10-26") 4800
  modifyResponse . foldr (.) id $ [
      addResponseCookie (expire name) . deleteResponseCookie name
      | name <- Map.keys (rqQueryParams req)
    ]
  redirect "/cookies"

setCookies = do
  params <- rqQueryParams <$> getRequest
  modifyResponse . foldr (.) id . map addResponseCookie $
    [Cookie k v Nothing Nothing (Just "/") False False
     | (k,vs) <- Map.toList params, v <- vs]
  redirect "/cookies"

listCookies = do
  cks <- rqCookies <$> getRequest
  let cs = [(decodeUtf8 (cookieName c),
             toJSON (decodeUtf8 (cookieValue c))) | c <- cks]
  respond $ \obj -> return $ obj <> [("cookies", object cs)]

redirect_ = do
  req <- getRequest
  let n   = fromMaybe (-1::Int) . rqIntParam "n" $ req
      prefix = B.reverse . B.dropWhile (/='/') . B.reverse . rqURI $ req
  case undefined of
    _| n > 1     -> redirect $ prefix <> pack (show (n-1))
     | n == 1    -> redirect "/get"
     | otherwise -> modifyResponse $ setResponseCode 400

unauthorized = modifyResponse $
               setHeader "WWW-Authenticate" "Basic realm=\"Fake Realm\"" .
               setResponseCode 401

simpleAuth expect = do
  req <- getRequest
  case expect req of
    Nothing -> modifyResponse $ setResponseCode 400
    Just (expected, resp) ->
      case getHeader "Authorization" (headers req) of
        Nothing -> unauthorized
        Just auth | auth == expected -> writeJSON $
                                        resp <> [("authenticated", Bool True)]
                  | otherwise -> unauthorized

basicAuth = simpleAuth $ \req ->
  case (rqParam "user" req, rqParam "pass" req) of
    (Just [user], Just [passwd]) | not (':' `B.elem` user) ->
      Just ("Basic " <> B64.encode (user <> ":" <> passwd),
            [("user", toJSON (B.unpack user))])
    _ -> Nothing

oauth2token = simpleAuth $ \req ->
  case (rqParam "kind" req, rqParam "token" req) of
    (Just [kind], Just [token]) ->
      Just (kind <> " " <> token,
            [("token", toJSON (B.unpack token))])
    _ -> Nothing

cache = do
  hdrs <- headers <$> getRequest
  let cond = not . null . catMaybes . map (flip getHeader hdrs) $
             ["If-Modified-Since", "If-None-Match"]
  if cond
    then modifyResponse $ setResponseCode 304
    else do
      now <- liftIO $ formatHttpTime =<< epochTime
      uuid <- liftIO nextRandom
      modifyResponse $ setHeader "Last-Modified" now .
                       setHeader "ETag" (toASCIIBytes uuid)
      respond return

rqIntParam name req =
  case rqParam name req of
    Just (str:_) -> case decimal (decodeUtf8 str) of
                      Right (n, "") -> Just n
                      _             -> Nothing
    _            -> Nothing

writeJSON obj = do
  modifyResponse $ setContentType "application/json"
  writeLBS . (<> "\n") . encodePretty' (Config 2 compare) . object $ obj

respond act = do
  req <- getRequest
  let step m k v = Map.insert (decodeUtf8 k) (decodeUtf8 (head v)) m
      params = Map.foldlWithKey' step Map.empty .
               rqQueryParams $ req
      wibble (k,v) = (decodeUtf8 (original k), decodeUtf8 v)
      rqHeaders = headers req
      hdrs = Map.fromList . map wibble . listHeaders $ rqHeaders
      url = case getHeader "Host" rqHeaders of
              Nothing   -> []
              Just host -> [("url", toJSON . decodeUtf8 $
                                    "http://" <> host <> rqURI req)]
  writeJSON =<< act ([ ("args", toJSON params)
                     , ("headers", toJSON hdrs)
                     , ("origin", toJSON . decodeUtf8 . rqRemoteAddr $ req)
                     ] <> url)

meths ms h = methods ms (path "" h)
meth m h   = method m (path "" h)

serve mkConfig = do
  cfg <- mkConfig
       . setAccessLog ConfigNoLog
       . setErrorLog ConfigNoLog
       $ defaultConfig
  httpServe cfg $ route [
      ("/get", meths [GET,HEAD] get)
    , ("/post", meth POST post)
    , ("/put", meth PUT put)
    , ("/delete", meth DELETE delete)
    , ("/patch", meth PATCH patch)
    , ("/redirect/:n", redirect_)
    , ("/status/:val", status)
    , ("/gzip", meths [GET,HEAD] gzip)
    , ("/cookies/delete", meths [GET,HEAD] deleteCookies)
    , ("/cookies/set", meths [GET,HEAD] setCookies)
    , ("/cookies", meths [GET,HEAD] listCookies)
    , ("/basic-auth/:user/:pass", meths [GET,HEAD] basicAuth)
    , ("/oauth2/:kind/:token", meths [GET,HEAD] oauth2token)
    , ("/cache", meths [GET,HEAD] cache)
    ]
