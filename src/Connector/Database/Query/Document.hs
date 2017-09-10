module Connector.Database.Query.Document
  ( getRows
  ) where

import Database.MySQL.Simple

import Ast
import Connector.Database.Query.Main (buildQuery)
import Entity.Document as Document
import Entity.Main

columns :: [String]
columns = fields $ Document 0 "" 0

getRows :: Connection -> Filter -> Entity -> IO [Entity]
getRows connection filter entity = do
  rows <- query_ connection (buildQuery "documents" columns filter entity) :: IO [(Int, String, Int)]
  return $ map (\(id, title, status) -> DocumentEntity $ Document id title status) rows
