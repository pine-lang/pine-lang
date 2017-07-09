{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import qualified Data.ByteString.Char8 as BS

import Data.ByteString.Lazy.Internal
import Network.Wreq
import System.Environment

getHeaders :: IO Options
getHeaders = do
  authToken <- BS.readFile "/home/mandark/.penneo-auth-token-local"
  return $ defaults & header "x-auth-token" .~ [authToken] &
    header "Authorization" .~
    ["JWT"]

request :: String -> IO (Response ByteString)
request url = do
  headers <- getHeaders
  getWith headers url

main :: IO ()
main = do
  r <- request "http://172.17.42.1:8000/app_dev.php/api/v1/casefiles"
  -- headers <- getHeaders
  -- r <- getWith headers "http://172.17.42.1:8000/app_dev.php/api/v1/casefiles"
  print $ r ^. responseStatus . statusCode
