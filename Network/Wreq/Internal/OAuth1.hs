{-# LANGUAGE OverloadedStrings #-}

module Network.Wreq.Internal.OAuth1
  (
   signRequest
  ) where

import Network.HTTP.Client (Request(..))
import Network.Wreq.Internal.Types (Auth(..))
import Web.Authenticate.OAuth ( signOAuth
                              , newOAuth
                              , oauthConsumerKey
                              , oauthConsumerSecret
                              , newCredential
                              , oauthCallback
                              , emptyCredential
                              , injectVerifier
                              , insert)


signRequest :: Auth -> Request -> IO Request
signRequest (OAuth1 consumerToken consumerSecret token tokenSecret) requestToSign = signOAuth app creds requestToSign
  where
    app   = newOAuth { oauthConsumerKey = consumerToken, oauthConsumerSecret = consumerSecret }
    creds = newCredential token tokenSecret
signRequest (OAuth1Temp consumerToken consumerSecret callbackUri) requestToSign = signOAuth app creds requestToSign
  where
    app   = newOAuth { oauthConsumerKey = consumerToken
                     , oauthConsumerSecret = consumerSecret
                     , oauthCallback = Just callbackUri }
    creds = insert "oauth_callback" callbackUri emptyCredential
signRequest (OAuth1ReqAccessToken consumerToken consumerSecret requestToken requestTokenSecret oauthVerifier) requestToSign
  = signOAuth app creds requestToSign
    where
      app   = newOAuth { oauthConsumerKey = consumerToken
                       , oauthConsumerSecret = consumerSecret }
      creds = injectVerifier oauthVerifier $ newCredential requestToken requestTokenSecret
signRequest _ requestToSign = return requestToSign
