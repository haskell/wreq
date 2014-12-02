{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveGeneric,
    OverloadedStrings, RecordWildCards #-}

module Network.Wreq.Cache
    (
      shouldCache
    , validateEntry
    , cacheStore
    ) where

import Control.Applicative
import Control.Lens ((^?), (^.), (^..), folded, non, pre, to)
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8 as A
import Data.CaseInsensitive (mk)
import Data.Foldable (forM_)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import Data.IORef (newIORef)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Monoid (First(..), mconcat)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (parseTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types (HeaderName, Method)
import Network.Wreq.Internal.Lens
import Network.Wreq.Internal.Types
import Network.Wreq.Lens
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import qualified Network.Wreq.Cache.Store as Store

#if MIN_VERSION_base(4,6,0)
import Data.IORef (atomicModifyIORef')
#else
import Data.IORef (atomicModifyIORef)

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' = atomicModifyIORef
#endif

cacheStore :: Int -> IO (Run body -> Run body)
cacheStore capacity = do
  cache <- newIORef (Store.empty capacity)
  return $ \run req -> do
    let url = reqURL req
    before <- getCurrentTime
    mresp <- atomicModifyIORef' cache $ \s ->
      case Store.lookup url s of
        Nothing       -> (s, Nothing)
        Just (ce, s') ->
          case validateEntry before ce of
            n@Nothing -> (Store.delete url s, n)
            resp      -> (s', resp)
    case mresp of
      Just resp -> return resp
      Nothing -> do
        resp <- run req
        after <- getCurrentTime
        forM_ (shouldCache after req resp) $ \ce ->
          atomicModifyIORef' cache $ \s -> (Store.insert url ce s, ())
        return resp

cacheableStatuses :: IntSet
cacheableStatuses = IntSet.fromList [200, 203, 300, 301, 410]

cacheableMethods :: HashSet Method
cacheableMethods = HashSet.fromList ["GET", "HEAD", "OPTIONS"]

possiblyCacheable :: Request -> Response body -> Bool
possiblyCacheable req resp =
    (req ^. method) `HashSet.member` cacheableMethods &&
    (resp ^. responseStatus . statusCode) `IntSet.member` cacheableStatuses

computeExpiration :: UTCTime -> [CacheResponse Seconds] -> Maybe UTCTime
computeExpiration now crs = do
  guard $ and [NoCache [] `notElem` crs, NoStore `notElem` crs]
  age <- listToMaybe $ sort [age | MaxAge age <- crs]
  return $! fromIntegral age `addUTCTime` now

validateEntry :: UTCTime -> CacheEntry body -> Maybe (Response body)
validateEntry now CacheEntry{..} =
  case entryExpires of
    Nothing          -> Just entryResponse
    Just e | e > now -> Just entryResponse
    _                -> Nothing

shouldCache :: UTCTime -> Req -> Response body -> Maybe (CacheEntry body)
shouldCache now (Req _ req) resp = do
  guard (possiblyCacheable req resp)
  let crs = resp ^.. responseHeader "Cache-Control" . atto_ parseCacheResponse .
                     folded . to simplifyCacheResponse
      dateHeader name = responseHeader name . to parseDate . folded
      mexpires = case crs of
                   [] -> resp ^? dateHeader "Expires"
                   _  -> computeExpiration now crs
      created = resp ^. pre (dateHeader "Date") . non now
  case mexpires of
    Just expires | expires <= created                -> empty
    Nothing      | req ^. method == "GET" &&
                   not (B.null (req ^. queryString)) -> empty
    _ -> return $ CacheEntry created mexpires resp

type Seconds = Int

data CacheResponse age = Public
                       | Private [HeaderName]
                       | NoCache [HeaderName]
                       | NoStore
                       | NoTransform
                       | MustRevalidate
                       | ProxyRevalidate
                       | MaxAge age
                       | SMaxAge age
                       | Extension
                       deriving (Eq, Show, Functor, Typeable, Generic)

instance Hashable age => Hashable (CacheResponse age)

simplifyCacheResponse :: CacheResponse age -> CacheResponse age
simplifyCacheResponse (Private _) = Private []
simplifyCacheResponse (NoCache _) = NoCache []
simplifyCacheResponse cr          = cr

parseCacheResponse :: A.Parser [CacheResponse Seconds]
parseCacheResponse = commaSep1 body
  where
    body = "public" *> pure Public
       <|> "private" *> (Private <$> (eq headerNames <|> pure []))
       <|> "no-cache" *> (NoCache <$> (eq headerNames <|> pure []))
       <|> "no-store" *> pure NoStore
       <|> "no-transform" *> pure NoTransform
       <|> "must-revalidate" *> pure MustRevalidate
       <|> "proxy-revalidate" *> pure ProxyRevalidate
       <|> "max-age" *> eq (MaxAge <$> decimal)
       <|> "s-maxage" *> eq (SMaxAge <$> decimal)
    headerNames = A.char '"' *> commaSep1 hdr <* A.char '"'
    hdr = mk <$> A.takeWhile1 (inClass "a-zA-Z0-9_-")
    commaSep1 p = (p <* skipSpace) `sepBy1` (A.char ',' *> skipSpace)
    eq p = skipSpace *> A.char '=' *> skipSpace *> p

parseDate :: B.ByteString -> Maybe UTCTime
parseDate s = getFirst . mconcat . map tryout $ [
    "%a, %d %b %Y %H:%M:%S %Z"
  , "%A, %d-%b-%y %H:%M:%S %Z"
  , "%a %b %e %H:%M:%S %Y"
  ]
  where tryout fmt = First $ parseTime defaultTimeLocale fmt (B.unpack s)
