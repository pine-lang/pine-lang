{-# LANGUAGE DeriveGeneric #-}

module Entity.SignatureLine
  ( SignatureLine(..)
  ) where

import Data.Aeson
import GHC.Generics

data SignatureLine = SignatureLine
  { id :: Int
  , role :: String
  , signerId :: Int
  , _documentId :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON SignatureLine

instance ToJSON SignatureLine
