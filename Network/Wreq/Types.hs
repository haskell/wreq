{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Network.Wreq.Types
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP client types.

module Network.Wreq.Types
    (
    -- * Client configuration
      Options(..)
    , Auth(..)
    , AWSAuthVersion(..)
    , ResponseChecker
    -- * Request payloads
    , Payload(..)
    , Postable(..)
    , Putable(..)
    -- ** URL-encoded forms
    , FormParam(..)
    , FormValue(..)
    -- * Headers
    , ContentType
    , Link(..)
    -- * Errors
    , JSONError(..)
    -- * Request handling
    , Req
    , reqURL
    , Run
    ) where

import Control.Lens ((&), (.~))
import Data.Aeson (Encoding, Value, encode)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Network.HTTP.Client (Request(method))
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import Network.Wreq.Internal.Types
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq.Internal.Lens as Lens

-- By default if the type is Putable, we use that as postPayload
instance Postable Part
instance Postable [Part]
instance Postable [(S.ByteString, S.ByteString)]
instance Postable (S.ByteString, S.ByteString)
instance Postable [FormParam]
instance Postable FormParam
instance Postable Payload
instance Postable S.ByteString
instance Postable L.ByteString
instance Postable Value
instance Postable Encoding

instance Putable Part where
    putPayload a = putPayload [a]

instance Putable [Part] where
    putPayload p req =
        -- According to doc, formDataBody changes the request type to POST which is wrong; change it back
        (\r -> r{method=method req}) `fmap` formDataBody p req

instance Putable [(S.ByteString, S.ByteString)] where
    putPayload ps req =
        -- According to doc, urlEncodedBody changes the request type to POST which is wrong; change it back
        return $ HTTP.urlEncodedBody ps req {method=method req}

instance Putable (S.ByteString, S.ByteString) where
    putPayload p = putPayload [p]

instance Putable [FormParam] where
    putPayload ps = putPayload (map f ps)
      where f (a := b) = (a, renderFormValue b)

instance Putable FormParam where
    putPayload p = putPayload [p]

instance Putable Payload where
    putPayload pl =
      case pl of
        Raw ct rb -> payload ct rb

instance Putable S.ByteString where
    putPayload = payload "application/octet-stream" . HTTP.RequestBodyBS

instance Putable L.ByteString where
    putPayload = payload "application/octet-stream" . HTTP.RequestBodyLBS

instance Putable Value where
    putPayload = payload "application/json" . HTTP.RequestBodyLBS . encode

instance Putable Encoding where
    putPayload = payload "application/json" . HTTP.RequestBodyLBS .
      encodingToLazyByteString

instance FormValue T.Text where
    renderFormValue = T.encodeUtf8

instance FormValue TL.Text where
    renderFormValue = T.encodeUtf8 . TL.toStrict

instance FormValue TL.Builder where
    renderFormValue = T.encodeUtf8 . TL.toStrict . TL.toLazyText

instance FormValue String where
    renderFormValue = T.encodeUtf8 . T.pack

instance FormValue S.ByteString where
    renderFormValue = id

instance FormValue L.ByteString where
    renderFormValue = S.concat . L.toChunks

instance FormValue Int where renderFormValue = renderFormValue . show
instance FormValue Int8 where renderFormValue = renderFormValue . show
instance FormValue Int16 where renderFormValue = renderFormValue . show
instance FormValue Int32 where renderFormValue = renderFormValue . show
instance FormValue Int64 where renderFormValue = renderFormValue . show
instance FormValue Integer where renderFormValue = renderFormValue . show

instance FormValue Word where renderFormValue = renderFormValue . show
instance FormValue Word8 where renderFormValue = renderFormValue . show
instance FormValue Word16 where renderFormValue = renderFormValue . show
instance FormValue Word32 where renderFormValue = renderFormValue . show
instance FormValue Word64 where renderFormValue = renderFormValue . show

instance FormValue Float where renderFormValue = renderFormValue . show
instance FormValue Double where renderFormValue = renderFormValue . show

instance FormValue () where renderFormValue _ = ""

instance (FormValue a) => FormValue (Maybe a) where
    renderFormValue (Just a) = renderFormValue a
    renderFormValue Nothing  = ""

payload :: S.ByteString -> HTTP.RequestBody -> Request -> IO Request
payload ct body req =
  return $ req & Lens.maybeSetHeader "Content-Type" ct
               & Lens.requestBody .~ body
