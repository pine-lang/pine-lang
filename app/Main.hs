{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Connector.Http.Http
import Connector.Database.Database

-- database
import Database.MySQL.Simple

import qualified Data.ByteString.Char8 as Char8
import System.Environment

-- import Data.Text as Text

-- data Config = Config
--   { tokenFile :: String
--   , baseUrl :: String
--   } deriving (Show)

-- config :: Config
-- config =
--   Config
--   { tokenFile = "/home/mandark/.penneo-auth-token-local"
--   , baseUrl = "http://dev.penneo.com:8000/app_dev.php/api/v1/"
--   }

-- main :: IO ()
-- main = do
--   token <- Char8.readFile (tokenFile config)
--   r <- run (baseUrl config) (Char8.unpack token)
--   print r

main :: IO ()
main = do
  conn <- connect connectInfo
  result <- run conn
  print $ result
  close conn

