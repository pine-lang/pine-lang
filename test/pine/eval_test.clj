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
      eval/build-query))

(deftest test-eval
  (testing "Build sql"
    (is (= {:query "SELECT * FROM \"company\" AS \"company_0\"  WHERE \"name\" = ?",
            :params ["Acme Inc."]}
           (generate "company | where: name='Acme Inc.'"))))
  (testing "Joins"
    (is (= {:query "SELECT * FROM \"company\" AS \"company_0\" JOIN \"employee\" AS \"employee_1\" ON \"company_0\".\"id\" = \"employee_1\".\"company_id\"",
            :params nil}
           (generate "company | employee")))))

