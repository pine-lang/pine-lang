-- This file contains Penneo specific details

module Connector.Database.Query
  ( columns
  -- Schema
  , Table
  , Column
  -- Columns
  , Id
  , Title
  , Status
  -- Records
  , CaseFile
  , Document
  , Entity(..)
  -- query
  , exec
  -- external
  , Connection
  -- testing
  , buildQuery
  ) where

import Ast
import Database.MySQL.Simple
import Data.List
import Debug.Trace
import Data.String
import Data.Tuple.Select

import qualified Control.Monad.State as StateMonad

-------------------------
-- Common Aliases
-------------------------

type Table = String
type Alias = String
type Column = String

-------------------------
-- Columns
-------------------------

type Id = Int
type Title = String
type Name = String
type Status = Int

-- Customer
type CustomerId = Int

-- Case File
type CaseFileId = Int

-- Signing Request
type SigningRequestId = Int
type Email = String
type EmailSubject = String

-- Signer
type SignerId = Int
type ValidatedName = String
type OnBehalfOf = String

type UserId = Int
type DocumentId = Int

-------------------------
-- Tables
-------------------------

type Customer = (CustomerId, Name)
type CaseFile = (CaseFileId, Title, UserId, CustomerId)
type Document = (DocumentId, Title, Status, CaseFileId)
type Signer = (SignerId, Name, Maybe ValidatedName, OnBehalfOf, Maybe UserId)
type SigningRequest = (SigningRequestId, Maybe Email, Maybe EmailSubject, Status, SignerId, CaseFileId)

-------------------------
-- Entities
-------------------------

data Entity
  =
    CustomerEntity Customer
  | CaseFileEntity CaseFile
  | DocumentEntity Document
  | SigningRequestEntity SigningRequest
  | SignerEntity Signer
  | NoEntity

instance Show Entity where
  show entity = case entity of
    CustomerEntity e       -> "\nCUSTOMER : " ++ show e
    CaseFileEntity e       -> "\nCASEFILE : " ++ show e
    DocumentEntity e       -> "\nDOCUMENT : " ++ show e
    SigningRequestEntity e -> "\nSIGNING REQUEST: " ++ show e
    SignerEntity e         -> "\nSIGNER : " ++ show e
    _                      -> "Can't show entity as it's show behavior isn't specified"


-------------------------
-- Helpers
-------------------------

belongsTo x y = elem (foreignKey y) (columns x)

type Schema = [(Table, [Alias], [Column])]

schema :: Schema
schema =
  [
    ("customers",       ["c", "cst"] , ["id", "name"]),
    ("caseFiles",       ["cf"]       , ["id", "title", "userId", "customerId"]),
    ("documents",       ["d", "docs"], ["id", "title", "status", "caseFileId"]),
    ("signingRequests", ["sr"],        ["id", "email", "emailSubject", "status", "signerId", "caseFileId"]),
    ("signers",         ["s"],         ["id", "name", "validatedName", "onBehalfOf", "userId"])
  ]

columns :: Table -> [Column]
columns table =
  sel3 $ head $ filter (\(t, _, _) -> t == table) schema

tableOfEntity entity = case entity of
     CustomerEntity       _ -> "customers"
     CaseFileEntity       _ -> "caseFiles"
     DocumentEntity       _ -> "documents"
     SigningRequestEntity _ -> "signingRequests"
     SignerEntity         _ -> "signers"
     NoEntity               -> ""

aliasToTable alias = sel1 $ head $ filter (\(table, aliases, _) -> table == alias || elem alias aliases) schema

getId entity = case entity of
     CustomerEntity r       -> show (sel1 r)
     CaseFileEntity r       -> show (sel1 r)
     DocumentEntity r       -> show (sel1 r)
     SigningRequestEntity r -> show (sel1 r)
     SignerEntity r         -> show (sel1 r)
     NoEntity               -> ""

foreignKey table = (take (length table - 1) table) ++ "Id"

-------------------------
-- Query Builder
-------------------------

select columns query = "SELECT x." ++ (intercalate ", x." columns) ++ query

from table = " FROM " ++ table ++ " AS x"

filterOn filter query =
  query ++
  case filter of
    Id id -> " AND x.id = " ++ (show id)
    Desc desc -> " AND x.title LIKE '%" ++ desc ++ "%'" -- FIXME: escape
    _ -> ""

joinWith t1 t2 query =
  query ++
  " JOIN " ++ t2 ++ " AS y"++
  " ON (x.id = y." ++ (foreignKey t1) ++ ")"

condition' column entity query =
  query ++ " WHERE " ++ column ++ " = " ++ (getId entity)


-- @todo: add functionality for joining on distant relationships
condition x entity query
  | x == "" || y == "" = query ++ " WHERE 1 "
  | x `belongsTo` y    = condition' ("x." ++ (foreignKey y)) entity query
  | y `belongsTo` x    = condition' "y.id" entity $ x `joinWith` y $ query
  | otherwise          = query ++ " WHERE NULL " -- no relationship found
  where y = tableOfEntity entity

limit l query = query ++ " LIMIT 10"

terminate query = query ++ ";"

buildQueryString :: Table -> [Column] -> Filter -> Entity -> String
buildQueryString table columns filter entity =
  terminate $
  limit "10" $
  filterOn filter $
  condition table entity $
  select columns $
  from table

buildQuery :: Table -> Filter -> Entity -> Query
buildQuery table filter entity =
  let query = buildQueryString table (columns table) filter entity
  in fromString (trace query query)

-------------------------
-- 'Get Rows' Functions
-------------------------

getCustomers :: Connection -> Filter -> Entity -> IO [Entity]
getCustomers connection filter entity = do
  rows <- query_ connection (buildQuery "customers" filter entity) :: IO [Customer]
  return $ map (\record -> CustomerEntity record) rows

getCaseFiles :: Connection -> Filter -> Entity -> IO [Entity]
getCaseFiles connection filter entity = do
  rows <- query_ connection (buildQuery "caseFiles" filter entity) :: IO [CaseFile]
  return $ map (\record -> CaseFileEntity record) rows

getDocuments :: Connection -> Filter -> Entity -> IO [Entity]
getDocuments connection filter entity = do
  rows <- query_ connection (buildQuery "documents" filter entity) :: IO [Document]
  return $ map (\record -> DocumentEntity record) rows

getSigningRequests :: Connection -> Filter -> Entity -> IO [Entity]
getSigningRequests connection filter entity = do
  rows <- query_ connection (buildQuery "signingRequests" filter entity) :: IO [SigningRequest]
  return $ map (\record -> SigningRequestEntity record) rows

getSigners :: Connection -> Filter -> Entity -> IO [Entity]
getSigners connection filter entity = do
  rows <- query_ connection (buildQuery "signers" filter entity) :: IO [Signer]
  return $ map (\record -> SignerEntity record) rows

getRows' :: Connection -> Table -> Filter -> Entity -> IO [Entity]
getRows' connection table filter entity = do
  case table of
    "customers" -> getCustomers connection filter entity
    "caseFiles" -> getCaseFiles connection filter entity
    "documents" -> getDocuments connection filter entity
    "signingRequests" -> getSigningRequests connection filter entity
    "signers" -> getSigners connection filter entity
    _ -> return []

getRows :: Connection -> Alias -> Filter -> Entity -> IO [Entity]
getRows connection alias filter entity =
  getRows' connection (aliasToTable alias) filter entity

-- How to make a generic 'getRows' function?

-- data Record = CaseFile | Document
-- type ContextEntity = Entity
--
-- getRows :: Connection -> Record -> Filter -> ContextEntity -> IO [Entity]
-- getRows connection record filter context = do
--   rows <- query_ connection (buildQuery (case record of
--                                            CaseFile -> "caseFiles"
--                                            Document -> "documents"
--                                         ) filter context) :: IO [Record]
--   return $ map (\r -> case record of
--                    CaseFile -> CaseFileEntity r
--                    Document -> DocumentEntity r
--                ) rows

exec'' :: Connection -> Table -> Filter -> Entity -> IO [Entity]
exec'' connection table filter entity =
  getRows connection table filter entity

exec' :: Connection -> Table -> Filter -> [Entity] -> IO [Entity]
exec' connection table filter entities = do
  result <- traverse (exec'' connection table filter) entities
  return $ StateMonad.join result

exec :: Connection -> Table -> Filter -> IO [Entity] -> IO [Entity]
exec connection table filter entities = do
  entity <- entities
  exec' connection table filter entity
