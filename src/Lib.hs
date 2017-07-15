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

type Id = Int

type Url = String

getOptions :: Token -> Options
getOptions token =
  defaults & (header "x-auth-token" .~ [Char8.pack token]) &
  (header "Authorization" .~ ["JWT"])

request :: (Url, Options) -> IO (Response ByteString)
request (url, options) = getWith options url

extract :: Response a -> a
extract response = response ^. responseBody

extractList :: Response [a] -> [a]
extractList response = response ^. responseBody

getCaseFile :: (Url, Options) -> Id -> IO CF.CaseFile
getCaseFile (baseUrl, options) id = do
  response <-
    asJSON =<< request (baseUrl ++ "casefiles/" ++ (show id), options) :: IO (Response CF.CaseFile)
  return $ extract response

getDocuments :: (Url, Options) -> CF.CaseFile -> IO [D.Document]
getDocuments (baseUrl, options) caseFile = do
  response <-
    asJSON =<<
    request
      ( baseUrl ++ "casefiles/" ++ (show (CF.id caseFile)) ++ "/documents"
      , options) :: IO (Response [D.Document])
  return $ extractList response

run :: Token -> Url -> IO [D.Document]
run token baseUrl =
  let options = getOptions token
  in do caseFile <- getCaseFile (baseUrl, options) 1
        documents <- getDocuments (baseUrl, options) caseFile
        return documents
  --
  -- return $ r
  -- return $ response^. responseStatus . statusCode
