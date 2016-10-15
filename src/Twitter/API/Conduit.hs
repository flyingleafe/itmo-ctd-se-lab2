{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
module Twitter.API.Conduit
  ( searchTweets
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
import           Data.Time.Clock          (getCurrentTime)

import           Twitter
import           Twitter.API
import           Twitter.Utils

type Query = ByteString
type Hashtag = ByteString
type Hours = Int
type CountPerPage = Maybe Int
type MaxTweetID = Maybe Integer

countTweetsLastHours :: MonadIO m => TwitterApp Authorized -> Hashtag -> Hours -> m [Int]
countTweetsLastHours app tag n = do
  curTime <- liftIO getCurrentTime
  let bottomTime = subtractHours (n - 1) $ roundToHours curTime

      accum [] tweet = [(roundToHours $ _createdAt tweet, 1)]
      accum ((hour, m) : hs) tweet =
        let curHour = roundToHours (_createdAt tweet)
        in if curHour == hour
           then (hour, m + 1) : hs
           else (curHour, 1) : (hour, m) : hs

  liftIO $ print bottomTime

  pairs <- runConduit $ searchTweets app ("#" <> tag)
    =$= C.takeWhile (\tweet -> _createdAt tweet > bottomTime)
    =$= C.foldl accum []

  return $ reverse $ map snd pairs

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

      unless (null tweets) $ do
        let newMaxId = Just $ minimum (map _id tweets) - 1
        searchTweetsBatch app q count newMaxId
