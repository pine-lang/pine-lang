{-# LANGUAGE OverloadedStrings #-}

module Connector.Database.Main
  ( execute
  ) where

import qualified Data.Map.Strict as Map

import Ast
import Control.Monad.State
import Connector.Database.Query as Query

-------------------------
-- Execute Operations
-------------------------

type Operation = (Table, Filter)
type PineValue = IO [Entity]
type PineState = IO [Entity]
execute' :: Connection -> [Operation] -> State PineState PineValue
execute' _ [] = do
  results <- get
  return results
execute' connection ((table, filter):operations) = do
  results <- get
  put (Query.exec connection table filter results)
  execute' connection operations

-- Nothing as the filter, and no results
initState :: IO [Entity]
initState = return [NoEntity]

-- Use test ops instead of the ops specified in arguments.
-- @todo: remove
--
testOps = [("caseFiles", Id 1), ("documents", Desc "Sample")]

execute :: Connection -> [Operation] -> IO [Entity]
execute connection ops = evalState (execute' connection ops) initState

-- Hard coded tests
--
-- execute :: Connection -> IO ([Entity])
-- execute connection = do
--   caseFiles <- execute' connection "caseFiles" (Id 1) NoEntity
--   documents <- execute' connection "documents" NoFilter (head caseFiles)
--   return $ documents
