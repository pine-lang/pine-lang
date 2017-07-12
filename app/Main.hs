module Main where

import Lib

import qualified Data.ByteString.Char8 as Char8
import System.Environment

data Config = Config
  { tokenFile :: String
  , baseUrl :: String
  } deriving (Show)

config :: Config
config =
  Config
  { tokenFile = "/home/mandark/.penneo-auth-token-local"
  , baseUrl = "http://dev.penneo.com:8000/app_dev.php/api/v1/"
  }

main :: IO ()
main = do
  token <- Char8.readFile (tokenFile config)
  r <- run (Char8.unpack token) (baseUrl config)
  print r
