{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RankNTypes                #-}
module Twitter where

import           Control.Monad              (guard)
import           Control.Monad.IO.Class
import           Data.Aeson.Types
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Network.HTTP.Simple        (Request, Response, httpLBS)

newtype Credential = Credential
  { crToken :: ByteString
  } deriving Show

instance FromJSON Credential where
  parseJSON = withObject "auth credentials" $ \o -> do
    tokenType <- o .: "token_type"
    guard $ tokenType == ("bearer" :: String)
    token <- o .: "access_token"
    return $ Credential $ BS.pack token

data TwitterApp a = forall b . APIBackend b => TwitterApp
  { consumerKey    :: ByteString
  , consumerSecret :: ByteString
  , authToken      :: Credential
  , backend        :: b
  }

-- Phantom types to encode authorized/unauthorized states of application
data Unauthorized
data Authorized

class AuthorizationState s
instance AuthorizationState Unauthorized
instance AuthorizationState Authorized

class APIBackend b where
  getAPIAnswer :: MonadIO m => b -> Request -> m (Response BSL.ByteString)

data HTTPBackend = HTTPBackend

instance APIBackend HTTPBackend where
  getAPIAnswer _ = httpLBS
