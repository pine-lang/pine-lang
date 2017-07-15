{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Internal
import qualified Model.CaseFile as CF
import qualified Model.Document as D
import Network.Wreq

-- Request related
type Token = String
type BaseUrl = String
type RelativeUrl = String
type Url = String
type Filter = Either Id Title
type Headers = Options -- wreq calls it Options, will change if doesn't make sense

-- Entity fields
type Id = Int
type Title = String

getHeaders :: Token -> Headers
getHeaders token =
  defaults & (header "x-auth-token" .~ [Char8.pack token]) &
  (header "Authorization" .~ ["JWT"])

request :: Url -> Headers -> IO (Response ByteString)
request url headers = getWith headers url

extract :: Response a -> a
extract response = response ^. responseBody

extractList :: Response [a] -> [a]
extractList response = response ^. responseBody

getUrl :: BaseUrl -> RelativeUrl -> Maybe Filter -> Url
getUrl base relative filter =
  base ++
  relative ++
  case filter of
    Just (Left id) -> "/" ++ (show id)
    Just (Right title) -> "?title=" ++ title
    _ -> ""

getCaseFiles :: BaseUrl -> Headers -> Maybe Filter -> IO CF.CaseFile
getCaseFiles baseUrl headers filter = do
  response <- asJSON =<< request (getUrl baseUrl "casefiles" filter) headers
  return $ extract response

getDocuments :: BaseUrl -> Headers -> CF.CaseFile -> IO [D.Document]
getDocuments baseUrl headers caseFile = do
  response <-
    asJSON =<<
    request
      (getUrl
         baseUrl
         ("casefiles/" ++ (show (CF.id caseFile)) ++ "/documents")
         Nothing)
      headers
  return $ extractList response

run :: Token -> BaseUrl -> IO [D.Document]
run token url =
  let headers = getHeaders token
  in do caseFile <- getCaseFiles url headers (Just (Left 1))
        documents <- getDocuments url headers caseFile
        return documents
  --
  -- return $ r
  -- return $ response^. responseStatus . statusCode
