module Connector.Database.Query
  ( buildQueryString
  , Condition(..)
  , Entity(..)
  ) where

import qualified Data.Map.Strict as Map
import Database.MySQL.Simple.Types
import Data.List

import qualified Model.CaseFile as CaseFile
import qualified Model.Document as Document

type Table = String
type Column = String

data Entity
  = CaseFileEntity CaseFile.CaseFile
  | DocumentEntity Document.Document
  | NoEntity
  deriving (Show)

data Condition
  = Id Int
  | Title String
  | SomeEntity Entity
  | NoCondition
  deriving (Show)

buildQueryString :: Table -> [Column] -> Condition -> String
buildQueryString table columns condition =
    "SELECT " ++
     (intercalate ", " columns) ++
     " FROM " ++
     table ++
     (case condition of
        Id id -> " WHERE id = " ++ (show id)
        Title title -> " WHERE title LIKE '" ++ title ++ "'" -- FIXME: excape
        SomeEntity (CaseFileEntity (CaseFile.CaseFile {CaseFile.id = id, CaseFile.title = title})) ->
          " WHERE caseFileId = " ++ (show id)
        _ -> " LIMIT 1 -- no condition found")

