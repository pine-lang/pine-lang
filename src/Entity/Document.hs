{-# LANGUAGE DeriveDataTypeable #-}

module Entity.Document
  ( Document(..)
  ) where

import Data.Data
import Text.Printf

data Document = Document
  { id :: Int
  , title :: String
  , status :: Int
  } deriving (Data)

instance Show Document where
  show (Document id title status) = printf "% 4d : % 25s : % 2d" id title status
