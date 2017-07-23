module Entity
  ( fields
  ) where

import Data.Data

fields :: Data a => a -> [String]
fields entity = constrFields $ toConstr entity

-- fields :: Constr -> [String]
-- fields c = constrFields c
