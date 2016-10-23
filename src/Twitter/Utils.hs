module Twitter.Utils where

import           Control.Exception      (try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor         (first)

import           Data.Aeson             (eitherDecode)
import           Data.Aeson.Types
import           Data.Time.Clock
import           Data.Time.Format       (defaultTimeLocale, parseTimeM)
import           Network.HTTP.Simple

import           Twitter

httpExceptionToString :: HttpException -> String
httpExceptionToString = show

tryJsonRequest :: (MonadIO m, FromJSON a, APIBackend b) => b -> Request -> m (Either String (Response a))
tryJsonRequest m req = do
  response <- getAPIAnswer m req
  return $ do
    when (getResponseStatusCode response /= 200)
      $ fail (show $ getResponseStatus response)
    traverse eitherDecode response

catchHTTPException :: IO (Either String a) -> IO (Either String a)
catchHTTPException action = join . first httpExceptionToString <$> try action

parseCreatedAt :: Monad m => String -> m UTCTime
parseCreatedAt = parseTimeM True defaultTimeLocale "%a %b %d %H:%M:%S %z %Y"

subtractHours :: Int -> UTCTime -> UTCTime
subtractHours n = addUTCTime (fromIntegral $ -n*3600)

roundToHours :: UTCTime -> UTCTime
roundToHours time = time { utctDayTime = secondsToDiffTime newSecs }
  where newSecs = (round (utctDayTime time) `div` 3600) * 3600

backNHours :: Int -> UTCTime -> [UTCTime]
backNHours n top = map (`subtractHours` roundedTop) [0..n-1]
  where roundedTop = roundToHours top

incHead :: Num a => [a] -> [a]
incHead []       = [1]
incHead (n : ns) = n + 1 : ns
