{-# LANGUAGE DeriveDataTypeable, GADTs, RecordWildCards #-}

module Network.WReq.Types
    (
      Options(..)
    , Auth(..)
    , ContentType
    , Payload(..)
    , JSONError(..)
    , Link(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Network.HTTP.Client (CookieJar, Manager, ManagerSettings,
                            destroyCookieJar)
import Network.HTTP.Client.Internal (Proxy)
import Network.HTTP.Client.MultipartFormData (Part)
import Network.HTTP.Types (Header)
import Prelude hiding (head)
import qualified Data.ByteString as S
import Data.Aeson (ToJSON)

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

data Payload where
    NoPayload :: Payload
    Raw       :: ContentType -> S.ByteString -> Payload
    Params    :: [(S.ByteString, S.ByteString)] -> Payload
    JSON      :: ToJSON a => a -> Payload
    FormData  :: [Part] -> Payload
  deriving (Typeable)

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

data Link = Link {
      linkURL :: S.ByteString
    , linkParams :: [(S.ByteString, S.ByteString)]
    } deriving (Eq, Show, Typeable)
