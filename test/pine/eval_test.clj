(ns pine.eval-test
  (:require [pine.eval :refer :all]
            [clojure.test :refer :all]
            [pine.ast.main :as ast]
            [pine.parser :as parser]
            [pine.eval :as eval]
            [pine.data-types :as dt]))

(defn- generate [expression]
  "Helper function to generate the sql"
  (-> expression
      parser/parse
      :result
      (ast/generate :test)
      eval/build-query))

(deftest test-build--query

  (testing "qualify table"
    (is (= "\"x\"" (q "x")))
    (is (= "\"x\".\"y\"" (q "x" "y"))))

  (testing "Condition"

    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" = ? LIMIT 250",
            :params (map dt/string ["Acme Inc."])}
           (generate "company | where: name='Acme Inc.'")))
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" LIKE ? AND \"c_0\".\"country\" = ? LIMIT 250",
            :params (map dt/string ["Acme%", "PK"])}
           (generate "company | where: name like 'Acme%' | country = 'PK'")))
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"country\" IN (?, ?) LIMIT 250",
            :params (map dt/string ["PK", "DK"])}
           (generate "company | where: country in ('PK' 'DK')"))))

  (testing "Condition with null"
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"deleted_at\" IS NULL LIMIT 250",
            :params []}
           (generate "company | where: deleted_at is null")))
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"deleted_at\" IS NOT NULL LIMIT 250",
            :params []}
           (generate "company | where: deleted_at is not null")))
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"deleted_at\" IS NULL LIMIT 250",
            :params []}
           (generate "company | where: deleted_at = null"))))

  (testing "Joins"
    (is (= {:query "SELECT e_1.* FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company | employee")))
    (is (= {:query "SELECT e_1.* FROM \"y\".\"company\" AS \"c_0\" JOIN \"x\".\"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "y.company | x.employee")))
    (is (= {:query "SELECT c_1.* FROM \"y\".\"employee\" AS \"e_0\" JOIN \"x\".\"company\" AS \"c_1\" ON \"e_0\".\"company_id\" = \"c_1\".\"id\" LIMIT 250",
            :params nil}
           (generate "y.employee | x.company")))
    (is (= {:query "SELECT d_2.* FROM \"x\".\"company\" AS \"c_0\" JOIN \"y\".\"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" JOIN \"z\".\"document\" AS \"d_2\" ON \"e_1\".\"id\" = \"d_2\".\"employee_id\" LIMIT 250",
            :params nil}
           (generate "x.company | y.employee | z.document"))))

  (testing "Joins with a context"
    (is (= {:query "SELECT d_2.* FROM \"x\".\"company\" AS \"c\" JOIN \"y\".\"employee\" AS \"e_1\" ON \"c\".\"id\" = \"e_1\".\"company_id\" JOIN \"z\".\"document\" AS \"d_2\" ON \"c\".\"id\" = \"d_2\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "x.company as c | y.employee | from: c | z.document"))))

  (testing "order"
    (is (= {:query "SELECT c_0.* FROM \"company\" AS \"c_0\" ORDER BY \"c_0\".\"country\" DESC LIMIT 250",
            :params nil}
           (generate "company | order: country"))))

  (testing "columns"
    (is (= {:query "SELECT \"c\".\"id\" FROM \"company\" AS \"c\" LIMIT 250",
            :params nil}
           (generate "company as c | select: id")))
    (is (= {:query "SELECT \"c_0\".\"id\" FROM \"company\" AS \"c_0\" LIMIT 250",
            :params nil}
           (generate "company | select: id")))
    (is (= {:query "SELECT \"c_0\".\"id\" AS \"c_id\" FROM \"company\" AS \"c_0\" LIMIT 250",
            :params nil}
           (generate "company | select: id as c_id")))
    (is (= {:query "SELECT \"c_0\".\"id\", e_1.* FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company | select: id | employee")))
    (is (= {:query "SELECT \"c_0\".\"id\", \"e_1\".\"id\" FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company | s: id | employee | s: id"))))

  (testing "delete"
    (is (= {:query "DELETE FROM \"company\" WHERE \"id\" IN ( SELECT \"c_0\".\"id\" FROM \"company\" AS \"c_0\" LIMIT 250 )",
            :params nil}
           (generate "company | delete! .id")))))

(deftest test-format--query
  (testing "string"
    (is (= "\nSELECT c_0.* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" = 'Acme Inc.' LIMIT 250;\n"
           (-> "company | where: name='Acme Inc.'" generate eval/formatted-query)))))
