-- @fixme: The AST doesn't contain to full AST

module Ast
  (  Filter(..)
  ) where

-- type Table = String
-- type Operation = (Table, Filter)
-- data Operations = Operation [Operation]

-- type Operation = ()
data Filter
  = Id Int
  | Desc String
  | NoFilter
  deriving (Show)
