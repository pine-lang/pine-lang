(ns pine.eval-test
  (:require [pine.eval :refer :all]
            [clojure.test :refer :all]
            [pine.ast.main :as ast]
            [pine.parser :as parser]
            [pine.eval :as eval]))

(defn- generate [expression]
  "Helper function to generate the sql"
  (-> expression
      parser/parse
      (ast/generate :test)
      eval/build-sql))

(deftest test-eval
  (testing "Build sql"
    (is (= {:sql "SELECT * FROM \"company\" AS \"company_0\"  WHERE \"name\" = ?",
            :params ["Acme Inc."]}
           (generate "company | where: name='Acme Inc.'")))))

