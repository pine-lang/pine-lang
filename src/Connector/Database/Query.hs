-- This file contains Penneo specific details

module Connector.Database.Query
  ( columns
  -- Schema
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
import Data.List
import Debug.Trace
import Data.String
import Data.Tuple.Select

-------------------------
-- Common Aliases
-------------------------

type Table = String
type Alias = String
type Column = String

-------------------------
-- Columns
-------------------------

type Id = Int
type Title = String
type Status = Int

type CaseFileId = Int
type UserId = Int
type DocumentId = Int

-------------------------
-- Tables
-------------------------

type CaseFile = (CaseFileId, Title, UserId)
type Document = (DocumentId, Title, Status, CaseFileId)

-------------------------
-- Entities
-------------------------

data Entity
  = CaseFileEntity CaseFile
  | DocumentEntity Document
  | NoEntity

instance Show Entity where
  show entity = case entity of
    CaseFileEntity e -> "\nCASEFILE : " ++ show e
    DocumentEntity e -> "\nDOCUMENT : " ++ show e
    _ -> "Can't show entity as it's show behavior isn't specified"


-------------------------
-- Helpers
-------------------------

hasForeignKey table associatedTo = elem (foreignKey associatedTo) (columns table)

type Schema = [(Table, [Alias], [Column])]

schema :: Schema
schema =
  [
    ("caseFiles", ["cf"]       , ["id", "title", "userId"]),
    ("documents", ["d", "docs"], ["id", "title", "status", "caseFileId"])
  ]

columns :: Table -> [Column]
columns table =
  sel3 $ head $ filter (\(t, _, _) -> t == table) schema

tableOfEntity entity = case entity of
     CaseFileEntity _ -> "caseFiles"
     DocumentEntity _ -> "documents"
     NoEntity -> ""

tableForAlias alias = sel1 $ head $ filter (\(table, aliases, _) -> table == alias || elem alias aliases) schema

getId entity = case entity of
     CaseFileEntity r -> show (sel1 r)
     DocumentEntity r -> show (sel1 r)
     NoEntity -> ""

foreignKey table = (take (length table - 1) table) ++ "Id"

-------------------------
-- Query Builder
-------------------------

select columns query = "SELECT x." ++ (intercalate ", x." columns) ++ query

from table = " FROM " ++ table ++ " AS x"

filterOn filter query =
  query ++
  " AND " ++
  case filter of
    Id id -> " x.id = " ++ (show id)
    Desc desc -> " x.title LIKE '%" ++ desc ++ "%'" -- FIXME: escape
    _ -> ""

join t1 t2 query =
  query ++
  " JOIN " ++ t2 ++ " AS y"++
  " ON (x.id = y." ++ (foreignKey t1) ++ ")"

condition' column entity query =
  query ++ " WHERE " ++ column ++ " = " ++ (getId entity)


condition t entity query
  | (tableOfEntity entity) == ""             = query ++ " WHERE 1 "
  | (hasForeignKey t (tableOfEntity entity)) = condition' ("x." ++ (foreignKey $ tableOfEntity entity)) entity query
  | otherwise                                = condition' "y.id" entity (join t (tableOfEntity entity) query)

limit l query = query ++ " LIMIT 10"

terminate query = query ++ ";"

buildQueryString :: Table -> [Column] -> Filter -> Entity -> String
buildQueryString table columns filter entity =
  terminate $
  limit "10" $
  filterOn filter $
  condition table entity $
  select columns $
  from table

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

getRows' :: Connection -> Table -> Filter -> Entity -> IO [Entity]
getRows' connection table filter entity = do
  case table of
    "caseFiles" -> getCaseFiles connection filter entity
    "documents" -> getDocuments connection filter entity
    _ -> return []

getRows :: Connection -> Alias -> Filter -> Entity -> IO [Entity]
getRows connection alias filter entity =
  getRows' connection (tableForAlias alias) filter entity

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
