{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
module Twitter.API.Conduit where

import           Control.Monad            (unless)
import           Control.Monad.IO.Class

import           Data.ByteString.Char8    (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Maybe               (fromMaybe)

import           Twitter
import           Twitter.API

type Query = ByteString
type CountPerPage = Maybe Int
type MaxTweetID = Maybe Integer

getCount :: CountPerPage -> Int
getCount = fromMaybe 15

searchTweets :: MonadIO m => TwitterApp Authorized -> Query -> Producer m Tweet
searchTweets app q = searchTweetsBatch app q Nothing Nothing =$= C.concat

searchTweetsBatch :: MonadIO m =>
                     TwitterApp Authorized
                  -> Query
                  -> CountPerPage
                  -> MaxTweetID
                  -> Producer m [Tweet]
searchTweetsBatch app q count maxId = do
  esearchRes <- call app APISearch (APISearchParams q count maxId Nothing)
  case esearchRes of
    Left _ -> return ()
    Right (APISearchVal tweets) -> do
      yield tweets

      let newMaxId = Just $ minimum (map _id tweets) - 1
          realCount = getCount count

      unless (length tweets < realCount) $
        searchTweetsBatch app q count newMaxId
