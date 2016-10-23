{-# LANGUAGE RecordWildCards #-}
module Twitter.OAuth
  ( TwitterApp
  , Unauthorized
  , Authorized
  , AuthorizationState
  , consumerKey
  , consumerSecret
  , authToken
  , makeApp
  , authorizeAppOnly
  , authorizedRequest
  ) where

import           Data.Monoid            ((<>))

import           Data.ByteString.Base64 (encode)
import           Data.ByteString.Char8  (ByteString)
import           Network.HTTP.Simple

import           Twitter
import           Twitter.Utils

makeApp :: ByteString -> ByteString -> TwitterApp Unauthorized
makeApp key secret = TwitterApp
  { consumerKey    = key
  , consumerSecret = secret
  , authToken      = error "Application is unauthorized yet!"
  , backend = HTTPBackend
  }

twitterHost :: ByteString
twitterHost = "api.twitter.com"

encodeSecret :: AuthorizationState a => TwitterApp a -> ByteString
encodeSecret app = encode $ consumerKey app <> ":" <> consumerSecret app

initAuthHeader :: AuthorizationState a => TwitterApp a -> ByteString
initAuthHeader app = "Basic " <> encodeSecret app

authHeader :: TwitterApp Authorized -> ByteString
authHeader app = "Bearer " <> crToken (authToken app)

appOnlyAuthRequest :: AuthorizationState a => TwitterApp a -> Request
appOnlyAuthRequest app =
    setRequestSecure True
  $ setRequestMethod "POST"
  $ setRequestPath "/oauth2/token"
  $ setRequestHost twitterHost
  $ setRequestPort 443
  $ addRequestHeader "Authorization" (initAuthHeader app)
  $ addRequestHeader "Content-Type" "application/x-www-form-urlencoded;charset=UTF-8"
  $ setRequestBody "grant_type=client_credentials" defaultRequest

authorizeAppOnly :: TwitterApp Unauthorized -> IO (Either String (TwitterApp Authorized))
authorizeAppOnly app@TwitterApp {..} = catchHTTPException $ do
  eresponse <- tryJsonRequest backend $ appOnlyAuthRequest app
  return $ do
    response <- eresponse
    return $ app { authToken = getResponseBody response }

authorizedRequest :: TwitterApp Authorized -> Request
authorizedRequest app =
    setRequestSecure True
  $ setRequestHost twitterHost
  $ setRequestPort 443
  $ addRequestHeader "Authorization" (authHeader app) defaultRequest
