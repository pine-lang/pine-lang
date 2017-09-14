-- This file contains Penneo specific details

module Connector.Database.Query
  ( columns
  -- Common Aliases
  , Table
  , Column
  -- Columns
  , Id
  , Title
  , Status
  -- Records
  , CaseFile
  , Document
  , Entity(..)
  -- query
  , getRows
  -- external
  , Connection
  ) where

import Ast
import Database.MySQL.Simple
-- import Database.MySQL.Simple.Types
import Data.List
import Debug.Trace
import Data.String

-------------------------
-- Common Aliases
-------------------------

type Table = String
type Column = String

-------------------------
-- Columns
-------------------------

type Id = Int
type Title = String
type Status = Int

-------------------------
-- Tables
-------------------------

type CaseFile = (Id, Title)
type Document = (Id, Title, Status)

-------------------------
-- Entities
-------------------------

data Entity
  = CaseFileEntity CaseFile
  | DocumentEntity Document
  | NoEntity

instance Show Entity where
  show entity = case entity of
    CaseFileEntity e -> "\n" ++ show e
    DocumentEntity e -> "\n" ++ show e
    _ -> "Can't show entity as it's show behavior isn't specified"


-------------------------
-- Helpers
-------------------------

columns :: Table -> [Column]
columns name = case name of
  "caseFiles" -> ["id", "title"]
  "documents" -> ["id", "title", "status"]
  _ -> [name ++ ".*"]

-------------------------
-- Query Builder
-------------------------

buildQueryString :: Table -> [Column] -> Filter -> Entity -> String
buildQueryString table columns filter entity =
  "SELECT " ++
  (intercalate ", " columns) ++
  " FROM " ++
  table ++
  " WHERE " ++
  (case filter of
     Id id -> " id = " ++ (show id)
     Desc desc -> " title LIKE '%" ++ desc ++ "%'" -- FIXME: escape
     _ -> "") ++
  (case entity of
     CaseFileEntity (id,title) -> " AND caseFileId = " ++ (show id)
     DocumentEntity (id,title,_) -> " AND documentId = " ++ (show id)
     NoEntity -> " /* no context */ ")
  ++ " LIMIT 10"

buildQuery :: Table -> Filter -> Entity -> Query
buildQuery table filter entity =
  let query = buildQueryString table (columns table) filter entity
  in fromString (trace query query)

-------------------------
-- 'Get Rows' Functions
-------------------------

getCaseFiles :: Connection -> Filter -> Entity -> IO [Entity]
getCaseFiles connection filter entity = do
  rows <- query_ connection (buildQuery "caseFiles" filter entity) :: IO [CaseFile]
  return $ map (\record -> CaseFileEntity record) rows

getDocuments :: Connection -> Filter -> Entity -> IO [Entity]
getDocuments connection filter entity = do
  rows <- query_ connection (buildQuery "documents" filter entity) :: IO [Document]
  return $ map (\record -> DocumentEntity record) rows

getRows :: Connection -> Table -> Filter -> Entity -> IO [Entity]
getRows connection table filter entity = do
  case table of
    "caseFiles" -> getCaseFiles connection filter entity
    "documents" -> getDocuments connection filter entity
    _ -> return []

-- How to make a generic 'getRows' function?

-- data Record = CaseFile | Document
-- type ContextEntity = Entity
--
-- getRows :: Connection -> Record -> Filter -> ContextEntity -> IO [Entity]
-- getRows connection record filter context = do
--   rows <- query_ connection (buildQuery (case record of
--                                            CaseFile -> "caseFiles"
--                                            Document -> "documents"
--                                         ) filter context) :: IO [Record]
--   return $ map (\r -> case record of
--                    CaseFile -> CaseFileEntity r
--                    Document -> DocumentEntity r
--                ) rows
