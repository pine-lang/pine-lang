{-# LANGUAGE DeriveDataTypeable #-}

module Entity.CaseFile
  ( CaseFile(..)
  , dummy
  ) where

import Data.Data

data CaseFile = CaseFile
  { id :: Int
  , title :: String
  } deriving (Show, Data)

dummy :: CaseFile
dummy = CaseFile 0 ""
