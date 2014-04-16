{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs, RecordWildCards #-}

module Network.WReq.Internal.Types
    (
      Options(..)
    , Auth(..)
    , ContentType
    , SimplePayload(..)
    , JSONError(..)
    , Link(..)
    , Put
    , Param
    , Putable(..)
    , Postable(..)
    ) where

import Control.Exception (Exception)
import Data.Aeson (ToJSON(toJSON), Value)
import Data.Typeable (Typeable)
import Network.HTTP.Client (CookieJar, Manager, ManagerSettings, Request,
                            destroyCookieJar)
import Network.HTTP.Client.Internal (Proxy)
import Network.HTTP.Types (Header)
import Prelude hiding (head)
import qualified Data.ByteString as S

type ContentType = S.ByteString

data Options = Options {
    manager :: Either ManagerSettings Manager
  , proxy :: Maybe Proxy
  , auth :: Maybe Auth
  , headers :: [Header]
  , params :: [(S.ByteString, S.ByteString)]
  , redirects :: Int
  , cookies :: CookieJar
  } deriving (Typeable)

data Auth = BasicAuth S.ByteString S.ByteString
          | OAuth2Bearer S.ByteString
          | OAuth2Token S.ByteString
          deriving (Eq, Show, Typeable)

instance Show Options where
  show (Options{..}) = concat ["Options { "
                              , "manager = ", case manager of
                                                Left _  -> "Left _"
                                                Right _ -> "Right _"
                              , ", proxy = ", show proxy
                              , ", auth = ", show auth
                              , ", headers = ", show headers
                              , ", params = ", show params
                              , ", redirects = ", show redirects
                              , ", cookies = ", show (destroyCookieJar cookies)
                              , " }"
                              ]

type Param = (S.ByteString, S.ByteString)

class Postable a where
    postPayload :: a -> Request -> IO Request

class Putable a where
    putPayload :: a -> Request -> IO Request

data SimplePayload where
    SimpleRaw       :: ContentType -> S.ByteString -> SimplePayload
    SimpleJSON      :: ToJSON a => a -> SimplePayload
  deriving (Typeable)

class Put a where
    _hidden :: a -> ()

instance Put S.ByteString where _hidden _ = ()
instance Put Value where _hidden _ = ()

instance Show SimplePayload where
    show (SimpleRaw contentType body) = "Raw " ++ show contentType ++ show body
    show (SimpleJSON js) = "JSON " ++ show (toJSON js)

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

data Link = Link {
      linkURL :: S.ByteString
    , linkParams :: [(S.ByteString, S.ByteString)]
    } deriving (Eq, Show, Typeable)
