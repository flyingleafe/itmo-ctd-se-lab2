module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Twitter.API
import           Twitter.OAuth

myApp :: TwitterApp Unauthorized
myApp = makeApp "wmcCp1Gnxh9n52ma6fjyNq0SH" "juMkmxHTekm6uQXzmR9NCEChONozviBSwaAzZEk73TedStApVM"

main :: IO ()
main = do
  eApp <- authorizeAppOnly myApp
  case eApp of
    Left err  -> putStrLn $ "Authorization error: " ++ err
    Right app -> do
      putStrLn "Authorized successfully"
      runConduit $ searchTweets app "#apple"
        =$= C.take 50
        =$= C.mapM_ print
