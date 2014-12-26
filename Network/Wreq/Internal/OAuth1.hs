module Network.Wreq.Internal.OAuth1
  (
   signRequest
  ) where

import Network.HTTP.Client (Request(..))
import Network.Wreq.Internal.Types (Auth(..))
import Web.Authenticate.OAuth ( signOAuth, newOAuth, oauthConsumerKey
                              , oauthConsumerSecret, newCredential)

signRequest :: Auth -> Request -> IO Request
signRequest (OAuth1 consumerToken consumerSecret token tokenSecret) requestToSign = signOAuth app creds requestToSign
  where
    app = newOAuth { oauthConsumerKey = consumerToken, oauthConsumerSecret = consumerSecret }
    creds = newCredential token tokenSecret

signRequest _ requestToSign = return requestToSign
