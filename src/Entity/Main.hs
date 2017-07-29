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
  deriving (Show)
