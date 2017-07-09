{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Internal
import Network.Wreq
import System.Environment

getHeaders :: String -> Options
getHeaders token =
  defaults & header "x-auth-token" .~ [Char8.pack token] &
  header "Authorization" .~
  ["JWT"]

request :: String -> String -> IO (Response ByteString)
request token url = getWith (getHeaders token) url

main :: IO ()
main = do
  token <- Char8.readFile "/home/mandark/.penneo-auth-token-local"
  r <-
    request
      (Char8.unpack token)
      "http://172.17.42.1:8000/app_dev.php/api/v1/casefiles"
  print $ r ^. responseStatus . statusCode
