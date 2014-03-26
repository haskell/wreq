{-# LANGUAGE TemplateHaskell #-}

module Network.WReq.Internal.Lens
    (
      HTTP.Request
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , proxy
    , hostAddress
    , rawBody
    , decompress
    , redirectCount
    , checkStatus
    , getConnectionWrapper
    , cookieJar
    ) where

import Lens.Family.TH (mkLensesBy)
import qualified Network.HTTP.Client as HTTP

mkLensesBy Just ''HTTP.Request
