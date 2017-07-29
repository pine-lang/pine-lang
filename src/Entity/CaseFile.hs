{-# LANGUAGE DeriveDataTypeable #-}

module Entity.CaseFile
  ( CaseFile(..)
  ) where

import Data.Data

data CaseFile = CaseFile
  { id :: Int
  , title :: String
  } deriving (Show, Data)
