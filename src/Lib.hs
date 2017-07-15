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

type Token = String

type BaseUrl = String

type RelativeUrl = String

type Url = String

type Filter = Either Id Title

type Id = Int

type Title = String

getOptions :: Token -> Options
getOptions token =
  defaults & (header "x-auth-token" .~ [Char8.pack token]) &
  (header "Authorization" .~ ["JWT"])

request :: Url -> Options -> IO (Response ByteString)
request url options = getWith options url

extract :: Response a -> a
extract response = response ^. responseBody

extractList :: Response [a] -> [a]
extractList response = response ^. responseBody

url :: BaseUrl -> RelativeUrl -> Maybe Filter -> Url
url base relative filter =
  base ++
  relative ++
  case filter of
    Just (Left id) -> "/" ++ (show id)
    Just (Right title) -> "?title=" ++ title
    _ -> ""

getCaseFiles :: BaseUrl -> Options -> Maybe Filter -> IO CF.CaseFile
getCaseFiles baseUrl options filter = do
  response <- asJSON =<< request (url baseUrl "casefiles" filter) options
  return $ extract response

getDocuments :: BaseUrl -> Options -> CF.CaseFile -> IO [D.Document]
getDocuments baseUrl options caseFile = do
  response <-
    asJSON =<<
    request
      (url
         baseUrl
         ("casefiles/" ++ (show (CF.id caseFile)) ++ "/documents")
         Nothing
      )
      options
  return $ extractList response

run :: Token -> BaseUrl -> IO [D.Document]
run token url =
  let options = getOptions token
  in do caseFile <- getCaseFiles url options (Just (Left 1))
        documents <- getDocuments url options caseFile
        return documents
  --
  -- return $ r
  -- return $ response^. responseStatus . statusCode
