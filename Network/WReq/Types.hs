{-# LANGUAGE DeriveDataTypeable, GADTs, RecordWildCards #-}

module Network.WReq.Types
    (
      Options(..)
    , ContentType
    , Payload(..)
    , JSONError(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, ManagerSettings)
import Network.HTTP.Client.Internal (Proxy)
import Network.HTTP.Types (Header)
import Prelude hiding (head)
import qualified Data.ByteString as S
import Data.Aeson (ToJSON)

type ContentType = S.ByteString

data Options = Options {
    manager :: Either ManagerSettings Manager
  , proxy :: Maybe Proxy
  , auth :: Maybe (S.ByteString, S.ByteString)
  , headers :: [Header]
  , params :: [(S.ByteString, S.ByteString)]
  } deriving (Typeable)

instance Show Options where
  show (Options{..}) = concat ["Options { "
                              , "manager = ", case manager of
                                                Left _  -> "Left _"
                                                Right _ -> "Right _"
                              , ", proxy = ", show proxy
                              , ", auth = ", show auth
                              , ", headers = ", show headers
                              , ", params = ", show params
                              , " }"
                              ]

data Payload where
    NoPayload :: Payload
    Raw       :: ContentType -> S.ByteString -> Payload
    Params    :: [(S.ByteString, S.ByteString)] -> Payload
    JSON      :: ToJSON a => a -> Payload
  deriving (Typeable)

data JSONError = JSONError String
               deriving (Show, Typeable)

instance Exception JSONError
