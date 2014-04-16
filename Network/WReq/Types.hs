{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.WReq.Types
    (
      Options(..)
    , Auth(..)
    , ContentType
    , SimplePayload(..)
    , JSONError(..)
    , Link(..)
    , Put
    , Postable(..)
    , Putable(..)
    ) where

import Control.Lens ((&), (.~))
import Data.Aeson (ToJSON(..), Value, encode)
import Network.HTTP.Client (Request)
import Network.HTTP.Client.MultipartFormData (Part, formDataBody)
import Network.WReq.Internal.Types
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

instance Postable SimplePayload where
    postPayload = simplePayload

instance Putable SimplePayload where
    putPayload = simplePayload

simplePayload :: SimplePayload -> Request -> IO Request
simplePayload payload req =
  case payload of
    SimpleRaw ct bs   -> return $ req & Int.setHeader "Content-Type" ct &
                         Int.requestBody .~ HTTP.RequestBodyBS bs
    SimpleJSON js    -> postPayload (toJSON js) req

instance Postable Value where
    postPayload js req =
      return $ req & Int.setHeader "Content-Type" "application/json" &
               Int.requestBody .~ HTTP.RequestBodyLBS (encode js)
