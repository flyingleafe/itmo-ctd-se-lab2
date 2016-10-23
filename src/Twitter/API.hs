{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Twitter.API where

import           Control.Monad          (join)
import           Control.Monad.IO.Class
import           Data.Maybe             (maybeToList)

import           Data.Aeson.Types
import           Data.ByteString.Char8  (ByteString, pack, unpack)
import           Data.Default
import           Data.Time.Clock        (UTCTime)
import           Network.HTTP.Simple

import           Twitter                (TwitterApp (..))
import           Twitter.OAuth
import           Twitter.Utils

class QueryParams p where
  toQueryParams :: p -> [(ByteString, Maybe ByteString)]
  fromQueryParams :: [(ByteString, Maybe ByteString)] -> Maybe p

class (QueryParams (Params m), FromJSON (Val m)) => APIMethod m where
  data Params m :: *
  data Val m :: *

  reqMethod :: m -> ByteString
  reqPath   :: m -> ByteString

makeRequest :: APIMethod m => TwitterApp Authorized -> m -> Params m -> Request
makeRequest app m p =
    setRequestMethod (reqMethod m)
  $ setRequestPath (reqPath m)
  $ setRequestQueryString (toQueryParams p)
  $ authorizedRequest app

call :: (MonadIO m, APIMethod a) => TwitterApp Authorized -> a -> Params a -> m (Either String (Val a))
call app@TwitterApp{..} m p = fmap getResponseBody <$> tryJsonRequest backend (makeRequest app m p)

callSafe :: APIMethod a => TwitterApp Authorized -> a -> Params a -> IO (Either String (Val a))
callSafe app m p = catchHTTPException $ call app m p

data Tweet = Tweet
  { _id        :: Integer
  , _createdAt :: UTCTime
  } deriving Show

instance FromJSON Tweet where
  parseJSON = withObject "tweet" $ \o -> Tweet <$> o .: "id" <*> (o .: "created_at" >>= parseCreatedAt)

data APISearch = APISearch

instance APIMethod APISearch where
  data Params APISearch = APISearchParams
    { _query   :: ByteString
    , _count   :: Maybe Int
    , _maxId   :: Maybe Integer
    , _sinceId :: Maybe Integer
    } deriving (Show)

  newtype Val APISearch = APISearchVal [Tweet] deriving Show

  reqMethod _ = "GET"
  reqPath _   = "/1.1/search/tweets.json"

instance QueryParams (Params APISearch) where
  toQueryParams p = concat [ [("q", Just $ _query p)]
                    , maybeToList $ intP "count" <$> _count p
                    , maybeToList $ intP "since_id" <$> _sinceId p
                    , maybeToList $ intP "max_id" <$> _maxId p]
    where intP str n = (str, Just $ pack $ show n)

  fromQueryParams params = APISearchParams
                           <$> join (lookup "q" params)
                           <*> parseLookup "count" params
                           <*> parseLookup "max_id" params
                           <*> parseLookup "since_id" params
    where parseLookup k p = return $ lookup k p >>= fmap (read . unpack)

instance Default (Params APISearch) where
  def = APISearchParams { _query   = error "default"
                        , _count   = Nothing
                        , _maxId   = Nothing
                        , _sinceId = Nothing
                        }

instance FromJSON (Val APISearch) where
  parseJSON = withObject "search results" $ \o -> APISearchVal <$> o .: "statuses"
