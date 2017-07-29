{-# LANGUAGE DeriveDataTypeable #-}

module Entity.Document
  ( Document(..)
  ) where

import Data.Data

data Document = Document
  { id :: Int
  , title :: String
  , status :: Int
  } deriving (Show, Data)

