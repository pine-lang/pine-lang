{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Model
  ( CaseFile
  , Document
  ) where

import Data.Aeson
import GHC.Generics

data CaseFile = CaseFile
  { id :: Int
  , title :: String
  } deriving (Show, Generic)

instance FromJSON CaseFile

instance ToJSON CaseFile

data Document = Document
  { id :: Int
  , title :: String
  } deriving (Show, Generic)

instance FromJSON Document

instance ToJSON Document
