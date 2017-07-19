{-# LANGUAGE OverloadedStrings #-}

module Connector.Database.Database
  ( run
  ) where

import qualified Data.Map.Strict as Map

import Database.MySQL.Simple
import Data.String
import Data.List

import Connector.Database.Query

import Model.CaseFile as CaseFile
import Model.Document as Document

rowToEntity :: String -> (Int, String) -> Entity
rowToEntity table (id, title) =
  case table of
    "documents" ->
      (DocumentEntity
         (Document.Document {Document.id = id, Document.title = title}))
    "casefiles" ->
      (CaseFileEntity
         (CaseFile.CaseFile {CaseFile.id = id, CaseFile.title = title}))
    _ -> NoEntity

getRows :: Connection -> String -> Condition -> IO [Entity]
getRows connection table condition = do
  rows <-
    query_
      connection
      (fromString $ buildQueryString table ["id", "title"] condition)
  return $ map (rowToEntity table) rows

run :: Connection -> IO ([Entity])
run connection = do
  caseFiles <- getRows connection "caseFiles" (Id 1)
  documents <- getRows connection "documents" (SomeEntity (head caseFiles))
  return $ documents
