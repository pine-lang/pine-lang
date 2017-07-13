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

getOptions :: String -> Options
getOptions token =
  defaults & (header "x-auth-token" .~ [Char8.pack token]) &
  (header "Authorization" .~ ["JWT"])

request :: String -> Options -> IO (Response ByteString)
request url options = getWith options url

extract :: Response [a] -> [a]
extract response = response ^. responseBody

getDocuments :: String -> Options -> CF.CaseFile -> IO [D.Document]
getDocuments baseUrl options caseFile = do
  response <-
    asJSON =<<
    request
      (baseUrl ++ "casefiles/" ++ (show (CF.id caseFile)) ++ "/documents")
      options :: IO (Response [D.Document])
  return $ extract response

run :: String -> String -> IO [D.Document]
run token baseUrl = do
  caseFiles <-
    asJSON =<< request (baseUrl ++ "casefiles") (getOptions token) :: IO (Response [CF.CaseFile])
  -- return $ r
  -- return $ response^. responseStatus . statusCode
  documents <-
    getDocuments baseUrl (getOptions token) (head $ extract caseFiles)
  return $ documents
