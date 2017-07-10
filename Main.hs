{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Internal
import GHC.Generics
import Network.Wreq
import System.Environment

data Config = Config
  { tokenFile :: String
  , baseUrl :: String
  } deriving (Show)

data CaseFile = CaseFile
  { id :: Int
  , title :: String
  } deriving (Show, Generic)

instance FromJSON CaseFile

instance ToJSON CaseFile

config :: Config
config =
  Config
  { tokenFile = "/home/mandark/.penneo-auth-token-local"
  , baseUrl = "http://dev.penneo.com:8000/app_dev.php/api/v1/"
  }

getHeaders :: String -> Options
getHeaders token =
  defaults & (header "x-auth-token" .~ [Char8.pack token]) &
  (header "Authorization" .~ ["JWT"])

url :: String -> String
url relativeUrl = (baseUrl config) ++ relativeUrl

request :: String -> String -> IO (Response ByteString)
request token url =
  let headers = getHeaders token
  in getWith (getHeaders token) url

main :: IO ()
main = do
  authToken <- Char8.readFile $ tokenFile config
  r <-
    asJSON =<< request (Char8.unpack authToken) (url "casefiles") :: IO (Response [CaseFile])
  -- print $ r
  -- print $ r ^. responseStatus . statusCode
  print $ r ^. responseBody
