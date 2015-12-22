module Network.Wreq.Internal.OAuth1 (signRequest) where

import Network.HTTP.Client (Request(..))
import Web.Authenticate.OAuth ( signOAuth, newOAuth, oauthConsumerKey
                              , oauthConsumerSecret, newCredential)
import qualified Data.ByteString as S

signRequest :: S.ByteString -> S.ByteString -> S.ByteString -> S.ByteString -> Request -> IO Request
signRequest consumerToken consumerSecret token tokenSecret = signOAuth app creds
  where
    app = newOAuth { oauthConsumerKey = consumerToken, oauthConsumerSecret = consumerSecret }
    creds = newCredential token tokenSecret
