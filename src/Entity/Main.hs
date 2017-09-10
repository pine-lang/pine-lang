module Entity.Main
  ( fields
  , Entity(..)
  ) where

import Data.Data

import Entity.CaseFile as CaseFile
import Entity.Document as Document

fields :: Data a => a -> [String]
fields entity = constrFields $ toConstr entity

data Entity
  = CaseFileEntity CaseFile.CaseFile
  | DocumentEntity Document.Document
  | NoEntity

instance Show Entity where
  show entity = case entity of
    CaseFileEntity e -> "\n" ++ show e
    DocumentEntity e -> "\n" ++ show e
    _ -> "Can't show entity as it's show behaviour isn't specified"
