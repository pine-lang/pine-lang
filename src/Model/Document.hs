{-# LANGUAGE DeriveGeneric #-}

module Model.Document
  ( Document(..)
  , fields
  ) where

import Data.Aeson
import GHC.Generics

data Document = Document
  { id :: Int
  , title :: String
  } deriving (Show, Generic)

instance FromJSON Document

instance ToJSON Document

fields :: [String]
fields = ["id", "title"]
