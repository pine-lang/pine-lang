module Ast
  ( Filter(..)
  , Operations
  ) where

data Filter
  = Id Int
  | Desc String
  | NoFilter
  deriving (Show)

type Table = String
type Operation = (Table, Filter)
type Operations = [Operation]
