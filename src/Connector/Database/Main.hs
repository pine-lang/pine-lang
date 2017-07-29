{-# LANGUAGE OverloadedStrings #-}

module Connector.Database.Main
  ( run
  ) where

import qualified Data.Map.Strict as Map

import Database.MySQL.Simple
import Data.List

-- import Ast
-- import Control.Monad.State

import Entity.Main
import Entity.CaseFile as CaseFile
import Entity.Document as Document

import Connector.Database.Query.Main
import Connector.Database.Query.CaseFile as QueryCaseFile
import Connector.Database.Query.Document as QueryDocument

type Table = String
-- type Operation = (Table, Filter)

runQuery :: Connection -> Table -> Filter -> Entity -> IO [Entity]
runQuery connection table filter entity = do
  entities <- case table of
    "caseFiles" -> QueryCaseFile.getRows connection filter entity
    "documents" -> QueryDocument.getRows connection filter entity
    _ -> return []
  return $ entities

-- type PineValue = [Entity]
-- type PineState = [Entity]
-- exec :: Connection -> [Operation] -> State PineState PineValue
-- exec _ [] = do
--   results <- get
--   return results
-- exec connection ((table, condition):operations) = do
--   results <- get
--   case results of
--     [] -> put (query table (conditionToFilter condition))
--     _ -> put (results >>= runQuery connection table)
--   exec operations


run :: Connection -> IO ([Entity])
run connection = do
  caseFiles <- runQuery connection "caseFiles" (Id 1) NoEntity
  documents <- runQuery connection "documents" NoFilter (head caseFiles)
  return $ documents
