{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Wreq.Internal.AWS
    (
      signRequest
    ) where

import Control.Applicative ((<$>))
import Control.Lens ((%~), (^.), (&), to)
import Crypto.MAC (hmac, hmacGetDigest)
import Data.ByteString.Base16 as HEX (encode)
import Data.Byteable (toBytes)
import Data.Char (toLower)
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (utc, utcToLocalTime)
import Network.HTTP.Types (parseSimpleQuery, urlEncode)
import Network.Wreq.Internal.Lens
import Network.Wreq.Internal.Types (AWSAuthVersion(..))
import System.Locale (defaultTimeLocale)
import qualified Crypto.Hash as CT (HMAC, SHA256)
import qualified Crypto.Hash.SHA256 as SHA256 (hash, hashlazy)
import qualified Data.ByteString.Char8 as S
import qualified Data.CaseInsensitive  as CI (original)
import qualified Data.HashSet as HashSet
import qualified Network.HTTP.Client as HTTP

-- Sign requests following the AWS v4 request signing specification:
-- http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
--
-- Runscope Inc. Traffic Inspector support:
-- We support (optionally) sending requests through the Runscope
-- (http://www.runscope.com) Traffic Inspector. If given a Runscope
-- URL to an AWS service, we will extract and correctly sign the
-- request for the underlying AWS service. We support Runscope buckets
-- with and without Bucket Authorization enabled
-- ("Runscope-Bucket-Auth").
--
-- TODO: adjust when DELETE supports a body or PATCH is added
signRequest :: AWSAuthVersion -> S.ByteString -> S.ByteString ->
               Request -> IO Request
signRequest AWSv4 = signRequestV4

signRequestV4 :: S.ByteString -> S.ByteString -> Request -> IO Request
signRequestV4 key secret request = do
  !ts <- timestamp                         -- YYYYMMDDT242424Z, UTC based
  let origHost = request ^. host          -- potentially w/ runscope bucket
      runscopeBucketAuth =
        lookup "Runscope-Bucket-Auth" $ request ^. requestHeaders
      noRunscopeHost = removeRunscope origHost -- rm Runscope for signing
      (service, region) = serviceAndRegion noRunscopeHost
      date = S.takeWhile (/= 'T') ts      -- YYYYMMDD
      hashedPayload
        | request ^. method `elem` ["POST", "PUT"] = payloadHash req
        | otherwise = HEX.encode $ SHA256.hash ""
      -- add common v4 signing headers, service specific headers, and
      -- drop tmp header and Runscope-Bucket-Auth header (if present).
      req = request & requestHeaders %~
            (([ ("host", noRunscopeHost)
              , ("x-amz-date", ts)] ++
              [("x-amz-content-sha256", hashedPayload) | service == "s3"]) ++)
            -- Runscope (correctly) doesn't send Bucket Auth header to AWS,
            -- remove it from the headers we sign. Adding back in at the end.
            . deleteKey "Runscope-Bucket-Auth"
  -- task 1
  let hl = req ^. requestHeaders . to sort
      signedHeaders = S.intercalate ";" . map (lowerCI . fst) $ hl
      canonicalReq = S.intercalate "\n" [
          req ^. method             -- step 1
        , req ^. path               -- step 2
        ,   S.intercalate "&"       -- step 3b, incl. sort
            -- urlEncode True (QS) to encode ':' and '/' (e.g. in AWS arns)
          . map (\(k,v) -> urlEncode True k <> "=" <> urlEncode True v)
          . sort $
          parseSimpleQuery $ req ^. queryString
        ,   S.unlines                -- step 4, incl. sort
          . map (\(k,v) -> lowerCI k <> ":" <> trimHeaderValue v) $ hl
        , signedHeaders             -- step 5
        , hashedPayload             -- step 6, handles empty payload
        ]
  -- task 2
  let dateScope = S.intercalate "/" [date, region, service, "aws4_request"]
      stringToSign = S.intercalate "\n" [
          "AWS4-HMAC-SHA256"
        , ts
        , dateScope
        , HEX.encode $ SHA256.hash canonicalReq
        ]
  -- task 3, steps 1 and 2
  let signature = ("AWS4" <> secret) &
                  hmac' date & hmac' region & hmac' service &
                  hmac' "aws4_request" & hmac' stringToSign & HEX.encode
      authorization = S.intercalate ", " [
          "AWS4-HMAC-SHA256 Credential=" <> key <> "/" <> dateScope
        , "SignedHeaders=" <> signedHeaders
        , "Signature=" <> signature
        ]
  -- Add the AWS Authorization header.
  -- Restore the Host header to the Runscope endpoint
  -- so they can proxy accordingly (if used, otherwise this is a nop).
  -- Add the Runscope Bucket Auth header back in, if it was set originally.
  return $ setHeader "host" origHost
        <$> maybe id (setHeader "Runscope-Bucket-Auth") runscopeBucketAuth
        <$> setHeader "authorization" authorization $ req
  where
    lowerCI = S.map toLower . CI.original
    trimHeaderValue =
      id -- FIXME, see step 4, whitespace trimming but not in double
         -- quoted sections, AWS spec.
    timestamp = render <$> getCurrentTime
      where render = S.pack . formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" .
                     utcToLocalTime utc -- UTC printable: YYYYMMDDTHHMMSSZ
    hmac' s k = toBytes (hmacGetDigest h)
      where h = hmac k s :: (CT.HMAC CT.SHA256)

payloadHash :: Request -> S.ByteString
payloadHash req =
  case HTTP.requestBody req of
    HTTP.RequestBodyBS bs ->
      HEX.encode $ SHA256.hash bs
    HTTP.RequestBodyLBS lbs ->
      HEX.encode $ SHA256.hashlazy lbs
    _ -> error "addTmpPayloadHashHeader: unexpected request body type"

-- Per AWS documentation at:
--   http://docs.aws.amazon.com/general/latest/gr/rande.html
-- For example: "dynamodb.us-east-1.amazonaws.com" -> ("dynamodb", "us-east-1")
serviceAndRegion :: S.ByteString -> (S.ByteString, S.ByteString)
serviceAndRegion endpoint
  -- For s3, use /<bucket> style access, as opposed to
  -- <bucket>.s3... in the hostname.
  | endpoint `elem` ["s3.amazonaws.com", "s3-external-1.amazonaws.com"] =
    ("s3", "us-east-1")
  | servicePrefix '-' endpoint == "s3" =
    -- format: e.g. s3-us-west-2.amazonaws.com
    let region = S.takeWhile (/= '.') $ S.drop 3 endpoint -- drop "s3-"
    in ("s3", region)
    -- not s3
  | endpoint `elem` ["sts.amazonaws.com"] =
    ("sts", "us-east-1")
  | svc `HashSet.member` noRegion =
    (svc, "us-east-1")
  | otherwise =
    let service:region:_ = S.split '.' endpoint
    in (service, region)
  where
    svc = servicePrefix '.' endpoint
    servicePrefix c = S.map toLower . S.takeWhile (/= c)
    noRegion = HashSet.fromList ["iam", "importexport", "route53", "cloudfront"]

-- If the hostname doesn't end in runscope.net, return the original.
-- For a hostname that includes runscope.net:
-- given  sqs-us--east--1-amazonaws-com-<BUCKET>.runscope.net
-- return sqs.us-east-1.amazonaws.com
removeRunscope :: S.ByteString -> S.ByteString
removeRunscope hostname
  | ".runscope.net" `S.isSuffixOf` hostname =
    S.concat . Prelude.map (p2 . p1) . S.group -- decode
    -- drop suffix "-<BUCKET>.runscope.net" before decoding
    . S.reverse . S.tail . S.dropWhile (/= '-') . S.reverse
    $ hostname
  | otherwise = hostname
    where p1 "-"   = "."
          p1 other = other
          p2 "--"   = "-"
          p2 other  = other
