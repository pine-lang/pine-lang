module Connector.Database.Query.CaseFile
  ( getRows
  ) where

import Database.MySQL.Simple

import Ast
import Connector.Database.Query.Main (buildQuery)
import Entity.CaseFile as CaseFile
import Entity.Main

columns :: [String]
columns = fields $ CaseFile 0 ""

getRows :: Connection -> Filter -> Entity -> IO [Entity]
getRows connection filter entity = do
  rows <- query_ connection (buildQuery "caseFiles" columns filter entity) :: IO [(Int, String)]
  return $ map (\(id, title) -> CaseFileEntity $ CaseFile id title) rows
