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

(deftest test-build--query
  (testing "qualify table"
    (is (= "\"x\"" (q "x")))
    (is (= "\"x\".\"y\"" (q "x" "y"))))
  (testing "Build sql"
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" = ?",
            :params ["Acme Inc."]}
           (generate "company | where: name='Acme Inc.'"))))

  (testing "Joins"
    (is (= {:query "SELECT e_1.* FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"e_1\".\"company_id\" = \"c_0\".\"id\"",
            :params nil}
           (generate "company | employee")))
    (is (= {:query "SELECT e_1.* FROM \"y\".\"company\" AS \"c_0\" JOIN \"x\".\"employee\" AS \"e_1\" ON \"e_1\".\"company_id\" = \"c_0\".\"id\"",
            :params nil}
           (generate "y.company | x.employee")))
    (is (= {:query "SELECT c_1.* FROM \"y\".\"employee\" AS \"e_0\" JOIN \"x\".\"company\" AS \"c_1\" ON \"e_0\".\"company_id\" = \"c_1\".\"id\"",
            :params nil}
           (generate "y.employee | x.company")))
    (is (= {:query "SELECT d_2.* FROM \"x\".\"company\" AS \"c_0\" JOIN \"y\".\"employee\" AS \"e_1\" ON \"e_1\".\"company_id\" = \"c_0\".\"id\" JOIN \"z\".\"document\" AS \"d_2\" ON \"d_2\".\"employee_id\" = \"e_1\".\"id\"",
            :params nil}
           (generate "x.company | y.employee | z.document"))))

  (testing "Build delete query"
    (is (= {:query "DELETE FROM x WHERE \"id\" IN ( SELECT c_0.* FROM \"company\" AS \"c_0\" ) as x",
            :params nil}
           (generate "company | delete! using id")))))
