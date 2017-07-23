{-# LANGUAGE DeriveGeneric #-}

module Entity.Signer
  ( Signer(..)
  ) where

import Data.Aeson
import GHC.Generics

data Signer = Signer
  { id :: Int
  , name :: String
  } deriving (Show, Generic)

instance FromJSON Signer

instance ToJSON Signer
