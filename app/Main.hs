{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Connector.Http.Http
import Connector.Database.Main
import Database.MySQL.Simple
import Parser


connectInfo :: ConnectInfo
connectInfo = ConnectInfo { connectHost = "?",
                            connectPort = 3306,
                            connectUser = "?",
                        connectPassword = "?",
                        connectDatabase = "?",
                         connectOptions = [],
                            connectPath = "",
                             connectSSL = Nothing }

arg = "caseFiles a | documents Sample | documents Sample"
-- arg = "documents Sample | caseFiles 1"
-- arg = "documents Sample | caseFiles 1"

main :: IO ()
main = do
  conn <- connect connectInfo
  result <- run conn (toAst arg)
  print $ result
  close conn

-- Test parsing of the input
-- @todo: move this to the tests
--
-- main :: IO ()
-- main = do
--   putStrLn $ show $ toAst "casefiles adf | documents 13 | signers asdf"


