{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
module Twitter.API.Conduit
  ( searchTweets
  , countTweetsPeriods
  , countTweetsLastHours
  , Query
  , Hashtag
  , CountPerPage
  , MaxTweetID
  ) where

import           Control.Monad            (unless)
import           Control.Monad.IO.Class
import           Data.Monoid              ((<>))

import           Data.ByteString.Char8    (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Time.Clock          (UTCTime, getCurrentTime)

import           Twitter
import           Twitter.API
import           Twitter.Utils

type Query = ByteString
type Hashtag = ByteString
type Hours = Int
type CountPerPage = Maybe Int
type MaxTweetID = Maybe Integer

countTweetsLastHours :: MonadIO m => TwitterApp Authorized -> Hashtag -> Hours -> m [Int]
countTweetsLastHours app tag n = liftIO getCurrentTime >>= countTweetsPeriods app tag . backNHours n

countTweetsPeriods :: MonadIO m => TwitterApp Authorized -> Hashtag -> [UTCTime] -> m [Int]
countTweetsPeriods app tag periods = do
  let minimumTime = last periods
      accum p@([], _) _ = p
      accum (hour : hs, counts) tweet
        | _createdAt tweet >= hour = (hour : hs, incHead counts)
        | otherwise                = accum (hs, 0 : counts) tweet

  pairs <- runConduit $ searchTweets app ("#" <> tag)
    =$= C.takeWhile (\tweet -> _createdAt tweet > minimumTime)
    =$= C.foldl accum (periods, [0])

  return $ reverse $ snd pairs

searchTweets :: MonadIO m => TwitterApp Authorized -> Query -> Producer m Tweet
searchTweets app q = searchTweetsBatch app q Nothing Nothing =$= C.concat

searchTweetsBatch
  :: MonadIO m
  => TwitterApp Authorized
  -> Query
  -> CountPerPage
  -> MaxTweetID
  -> Producer m [Tweet]
searchTweetsBatch app q count maxId = do
  esearchRes <- call app APISearch (APISearchParams q count maxId Nothing)
  case esearchRes of
    Left err -> liftIO $ putStrLn err
    Right (APISearchVal tweets) -> do
      yield tweets

      unless (null tweets) $ do
        let newMaxId = Just $ minimum (map _id tweets) - 1
        searchTweetsBatch app q count newMaxId
