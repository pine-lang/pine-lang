module Connector.Database.Query.Main
  ( buildQuery
  ) where

import qualified Data.Map.Strict as Map
import Database.MySQL.Simple.Types
import Data.List
import Data.String

import Ast
import Entity.Main
import qualified Entity.CaseFile as CaseFile
import qualified Entity.Document as Document

import Debug.Trace

type Table = String
type Column = String

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
     CaseFileEntity (CaseFile.CaseFile id title) -> " AND caseFileId = " ++ (show id)
     DocumentEntity (Document.Document id title _) -> " AND documentId = " ++ (show id)
     _ -> " /* unable to build query for context entity */ ") ++
  " LIMIT 10"


buildQuery :: Table -> [Column] -> Filter -> Entity -> Query
buildQuery table columns filter entity =
  let query = buildQueryString table columns filter entity
  in fromString (trace query query)
