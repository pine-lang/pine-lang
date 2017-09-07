{-# LANGUAGE OverloadedStrings #-}

module Connector.Database.Main
  ( run
  ) where

import qualified Data.Map.Strict as Map

import Database.MySQL.Simple
import Data.List
import Control.Monad.State

import Entity.Main
import Entity.CaseFile as CaseFile
import Entity.Document as Document

import Connector.Database.Query.Main
import Connector.Database.Query.CaseFile as QueryCaseFile
import Connector.Database.Query.Document as QueryDocument

import Debug.Trace

type Table = String

runQuery :: Connection -> Table -> Filter -> Entity -> IO [Entity]
runQuery connection table filter entity = do
  entities <- case table of
    "caseFiles" -> QueryCaseFile.getRows connection filter entity
    "documents" -> QueryDocument.getRows connection filter entity
    _ -> return []
  return $ entities

runQuery' :: Connection -> Table -> Filter -> [Entity] -> IO [Entity]
runQuery' connection table filter entities = do
  result <- traverse (runQuery connection table filter) entities
  return $ join result

runQuery'' :: Connection -> Table -> Filter -> IO [Entity] -> IO [Entity]
runQuery'' connection table filter entities = do
  result <- entities
  runQuery' connection table filter result

-- test :: IO [Entity]
-- test = do
--   return [CaseFileEntity (CaseFile.CaseFile 0 "dummy")]

type Operation = (Table, Filter)
type PineValue = IO [Entity]
type PineState = IO [Entity]
exec :: Connection -> [Operation] -> State PineState PineValue
exec _ [] = do
  results <- get
  return results
exec connection ((table, filter):operations) = do
  results <- get
  put (runQuery'' connection table filter results)
  exec connection operations

-- Nothing as the filter, and no results
initState :: IO [Entity]
initState = return [NoEntity]

ops = [("caseFiles", Id 1), ("documents", Desc "Sample")]

run :: Connection -> IO [Entity]
run connection = evalState (exec connection ops) initState

-- Hard coded tests
--
-- run :: Connection -> IO ([Entity])
-- run connection = do
--   caseFiles <- runQuery connection "caseFiles" (Id 1) NoEntity
--   documents <- runQuery connection "documents" NoFilter (head caseFiles)
--   return $ documents
