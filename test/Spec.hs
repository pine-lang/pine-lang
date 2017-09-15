import Test.Hspec
-- import Test.QuickCheck
import Text.ParserCombinators.Parsec
-- -- import Control.Exception (evaluate)

import Connector.Database.Main
import Parser

main :: IO ()
main =
  hspec $ do
    describe "Pine:" $ do

      it "two expressions: containing filters" $ do
        show (toAst "customers \"acme\" | users 1") `shouldBe`
          "[(\"customers\",Desc \"acme\"),(\"users\",Id 1)]"

      it "two expressions: one contains filter" $ do
        show (toAst "customers \"acme\" | users *") `shouldBe`
          "[(\"customers\",Desc \"acme\"),(\"users\",NoFilter)]"
