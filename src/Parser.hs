{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( toAst
  ) where

import Ast
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token -- lexeme functions

-- pipeSeparated = sepBy ((optional spaces) >> many1 letter >>= (\x -> (optional spaces) >> return x)) (char '|')

type Table = String


toFilter s = case reads s :: [(Integer, String)] of
  [(i, "")] -> Id (fromInteger i)
  _         -> Desc s

operation = do
  optional spaces
  entity <- many1 letter
  spaces
  arg <- try (many1 letter) <|> try (many1 digit)
  optional spaces
  return $ (entity, toFilter arg )

operations = sepBy operation (char '|')


-- change the operation type..
type Operations = [(Table, Filter)]

parseSql :: Operations -> Parser Operations
parseSql sql = do
  ops <- operations
  -- optional spaces
  -- entity
  return $ ops

-- casefiles 1 | documents "abc" | signers "John"

toAst :: String -> Operations
toAst input = case parse (parseSql []) "(source)" input of
    -- Left err -> "No match: " ++ show err
    Left err -> []
    Right val -> val

