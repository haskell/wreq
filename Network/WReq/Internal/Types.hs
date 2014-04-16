{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs, RecordWildCards #-}

module Network.WReq.Internal.Types
    (
      Options(..)
    , Auth(..)
    , ContentType
    , Payload(..)
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
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (CookieJar, Manager, ManagerSettings, Request,
                            destroyCookieJar)
import Network.HTTP.Client.Internal (Proxy)
import Network.HTTP.Client.MultipartFormData (Part)
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

data Payload a where
    Raw       :: ContentType -> S.ByteString -> Payload S.ByteString
    Params    :: [Param] -> Payload [Param]
    JSON      :: ToJSON a => a -> Payload Value
    FormData  :: [Part] -> Payload [Part]
  deriving (Typeable)

data SimplePayload where
    SimpleRaw       :: ContentType -> S.ByteString -> SimplePayload
    SimpleJSON      :: ToJSON a => a -> SimplePayload
  deriving (Typeable)

class Put a where
    _hidden :: a -> ()

instance Put S.ByteString where _hidden _ = ()
instance Put Value where _hidden _ = ()

instance Show (Payload a) where
    show (Raw contentType body) = "Raw " ++ show contentType ++ show body
    show (Params ps) = "Params " ++ show ps
    show (JSON js) = "JSON " ++ show (toJSON js)
    show (FormData fs) = "FormData " ++ show fs

instance Monoid (Payload [Param]) where
    mempty = Params []
    mappend (Params xs) (Params ys) = Params (xs ++ ys)

instance Monoid (Payload [Part]) where
    mempty = FormData []
    mappend (FormData xs) (FormData ys) = FormData (xs ++ ys)

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError

data Link = Link {
      linkURL :: S.ByteString
    , linkParams :: [(S.ByteString, S.ByteString)]
    } deriving (Eq, Show, Typeable)
