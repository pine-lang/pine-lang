{-# LANGUAGE DeriveGeneric #-}

module Model.CaseFile
  ( CaseFile(..)
  ) where

import Data.Aeson
import GHC.Generics

data CaseFile = CaseFile
  { id :: Int
  , title :: String
  } deriving (Show, Generic)

instance FromJSON CaseFile

instance ToJSON CaseFile
