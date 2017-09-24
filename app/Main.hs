{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Parse Input to AST
import Parser

-- Pine DB Connector
import qualified Connector.Database.Main as PineDbConnector
import Database.MySQL.Simple
import Config (connectInfo)

-- Web interface
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Char8 (unpack)

-- Handling SIGINT (Ctl-C)
import Control.Concurrent
import System.Exit
import System.Posix.Signals


-- arg = "caseFiles a | documents Sample"
arg = "d \"ample\" | cf \"ample\""

app :: Connection -> Application
app conn request respond = do
  input <- requestBody request
  result <- PineDbConnector.execute conn (toAst $ unpack input)
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] (pack $ show result)

handleShutdown :: ThreadId -> Connection -> IO ()
handleShutdown tid conn =
  putStrLn "\n" >>
  putStrLn "Cleaning up the db connection.. " >>
  close conn >>
  putStrLn "Done!\n" >>
  throwTo tid ExitSuccess

main :: IO ()
main = do
  conn <- connect connectInfo
  tid <- myThreadId
  installHandler sigINT (Catch $ handleShutdown tid conn) Nothing
  putStrLn $ "http://localhost:9901/"
  run 9901 (app conn)

