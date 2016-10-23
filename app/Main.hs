module Main where

import           Twitter.API.Conduit
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
      lastHoursCounts <- countTweetsLastHours app "haskell" 3
      print lastHoursCounts
