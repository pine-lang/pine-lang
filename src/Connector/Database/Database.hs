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

type Table = String

rowToEntity :: Table -> (Int, String) -> Entity
rowToEntity table (id, title) =
  case table of
    "documents" ->
      (DocumentEntity
         (Document.Document {Document.id = id, Document.title = title}))
    "casefiles" ->
      (CaseFileEntity
         (CaseFile.CaseFile {CaseFile.id = id, CaseFile.title = title}))
    _ -> NoEntity

buildQuery :: Table -> Filter -> Query
buildQuery table filter =
  fromString $
  buildQueryString
    table
    (case table of
       "caseFiles" -> CaseFile.fields
       "documents" -> Document.fields
       _ -> ["*"]
    )
    filter

getRows :: Connection -> Table -> Filter -> IO [Entity]
getRows connection table filter = do
  rows <- query_ connection (buildQuery table filter)
  return $ map (rowToEntity table) rows

run :: Connection -> IO ([Entity])
run connection = do
  caseFiles <- getRows connection "caseFiles" (Id 1)
  documents <- getRows connection "documents" (SomeEntity (head caseFiles))
  return $ documents
