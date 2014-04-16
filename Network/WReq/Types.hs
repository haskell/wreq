{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.WReq.Types
    (
      Options(..)
    , Auth(..)
    , ContentType
    , Payload(..)
    , JSONError(..)
    , Link(..)
    , Postable(..)
    , Putable(..)
    ) where

import Control.Lens ((&), (.~))
import Data.Aeson (Value, encode)
import Network.HTTP.Client (Request)
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import Network.WReq.Internal.Types
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as HTTP
import qualified Network.WReq.Lens.Internal as Int

instance Postable Part where
    postPayload a = postPayload [a]

instance Postable [Part] where
    postPayload = formDataBody

instance Postable [Param] where
    postPayload ps req = return $ HTTP.urlEncodedBody ps req

instance Postable Param where
    postPayload p = postPayload [p]

instance Postable Payload where
    postPayload = putPayload

instance Postable S.ByteString where
    postPayload = putPayload

instance Postable L.ByteString where
    postPayload = putPayload

instance Postable Value where
    postPayload = putPayload


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


payload :: ContentType -> HTTP.RequestBody -> Request -> IO Request
payload ct body req = return $ req & Int.setHeader "Content-Type" ct &
                      Int.requestBody .~ body
