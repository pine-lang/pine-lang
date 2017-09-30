import Test.Hspec
-- import Test.QuickCheck
import Text.ParserCombinators.Parsec
-- -- import Control.Exception (evaluate)

import Connector.Database.Main
import Parser

-- query
import Ast
import Connector.Database.Query


dummyCaseFile = CaseFileEntity $ Just (876, "sample", 1, 1)
dummyDocument = DocumentEntity $ Just (987, "sample", 1, 1)

main :: IO ()
main =
  hspec $ do
    describe "Pine:" $ do

      it "two expressions: containing filters" $ do
        show (toAst "customers \"acme\" | users 1") `shouldBe`
          "[(\"customers\",Desc \"acme\"),(\"users\",Id 1)]"
        -- @todo: use the following Filter is an instance of 'Eq'
        -- toAst "customers \"acme\" | users 1" `shouldBe`
        --   [("customers", Desc "acme"), ("users", Id 1)]

      it "two expressions: one contains filter" $ do
        show (toAst "customers \"acme\" | users *") `shouldBe`
          "[(\"customers\",Desc \"acme\"),(\"users\",NoFilter)]"

      it "penneo : build query: no relationship" $ do
        show (buildQuery "caseFiles" NoFilter NoEntity) `shouldBe`
          "\"SELECT x.id, x.title, x.userId, x.customerId FROM caseFiles AS x WHERE 1  LIMIT 10;\""

      it "penneo : build query: belongs to relationship" $ do
        show (buildQuery "documents" NoFilter dummyCaseFile) `shouldBe`
          "\"SELECT x.id, x.title, x.status, x.caseFileId FROM documents AS x WHERE x.caseFileId = 876 LIMIT 10;\""

      it "penneo : build query: inverse belongs to relationship" $ do
        show (buildQuery "caseFiles" NoFilter dummyDocument) `shouldBe`
          "\"SELECT x.id, x.title, x.userId, x.customerId FROM caseFiles AS x JOIN documents AS y ON (x.id = y.caseFileId) WHERE y.id = 987 LIMIT 10;\""
