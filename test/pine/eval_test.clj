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
    (is (= {:query "SELECT * FROM \"company\" AS \"company_0\" WHERE \"name\" = ?",
            :params ["Acme Inc."]}
           (generate "company | where: name='Acme Inc.'"))))

  (testing "Joins"
    (is (= {:query "SELECT * FROM \"company\" AS \"company_0\" JOIN \"employee\" AS \"employee_1\" ON \"employee_1\".\"company_id\" = \"company_0\".\"id\"",
            :params nil}
           (generate "company | employee")))
    (is (= {:query "SELECT * FROM \"company\" AS \"company_0\" JOIN \"employee\" AS \"employee_1\" ON \"employee_1\".\"company_id\" = \"company_0\".\"id\"",
            :params nil}
           (generate "y.company | x.employee")))
    (is (= {:query "SELECT * FROM \"employee\" AS \"employee_0\" JOIN \"company\" AS \"company_1\" ON \"employee_0\".\"company_id\" = \"company_1\".\"id\"",
            :params nil}
           (generate "y.employee | x.company")))
    (is (= {:query "SELECT * FROM \"company\" AS \"company_0\" JOIN \"employee\" AS \"employee_1\" ON \"employee_1\".\"company_id\" = \"company_0\".\"id\" JOIN \"document\" AS \"document_2\" ON \"document_2\".\"employee_id\" = \"employee_1\".\"id\"",
            :params nil}
           (generate "x.company | y.employee | z.document")))))
