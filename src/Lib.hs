{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Internal
import Model
import Network.Wreq

getHeaders :: String -> Options
getHeaders token =
  defaults & (header "x-auth-token" .~ [Char8.pack token]) &
  (header "Authorization" .~ ["JWT"])

request :: Options -> String -> IO (Response ByteString)
request headers url = getWith headers url

run :: String -> String -> IO ()
run token baseUrl = do
  r <-
    asJSON =<< request (getHeaders token) (baseUrl ++ "casefiles") :: IO (Response [CaseFile])
  -- -- print $ r
  -- -- print $ r ^. responseStatus . statusCode
  print $ r ^. responseBody
