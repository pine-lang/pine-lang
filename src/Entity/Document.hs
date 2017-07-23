{-# LANGUAGE DeriveDataTypeable #-}

module Entity.Document
  ( Document(..)
  , dummy
  ) where

import Data.Data

data Document = Document
  { id :: Int
  , title :: String
  } deriving (Show, Data)

dummy :: Document
dummy = Document 0 ""
