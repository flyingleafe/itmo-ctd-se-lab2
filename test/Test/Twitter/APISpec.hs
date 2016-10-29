module Test.Twitter.APISpec ( spec ) where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.List        (consume)
import           Data.Time.Clock
import           Data.Time.Format         (defaultTimeLocale, iso8601DateFormat,
                                           parseTimeM)
import           Test.Hspec

import           Twitter.API
import           Twitter.API.Conduit
import           Twitter.Utils

import           Test.Twitter

parseIsoTime :: Monad m => String -> m UTCTime
parseIsoTime = parseTimeM True defaultTimeLocale $ iso8601DateFormat (Just "%H:%M:%S")

spec :: Spec
spec = describe "Search API behaviour" $ do
  it "should return correct data" $ do
    list <- runConduit $ searchTweets mockApp "" =$= consume
    length list `shouldBe` 15

  it "should return correct slices of data" $ do
    time <- parseIsoTime "2016-10-23T08:00:00"
    list <- runConduit $ searchTweets mockApp ""
      =$= (C.drop 3 >> C.map id)
      =$= C.takeWhile (\t -> _createdAt t >= time)
      =$= consume
    length list `shouldBe` 4

  it "should return correct hour distribution" $ do
    time <- parseIsoTime "2016-10-23T13:30:00"
    let periods = backNHours 10 time
    distribution <- countTweetsPeriods mockApp "" periods
    distribution `shouldBe` [0, 3, 2, 0, 1, 1, 0, 2, 2, 4]

