(ns pine.eval-test
  (:require [clojure.test :refer [deftest is testing]]
            [pine.ast.main :as ast]
            [pine.parser :as parser]
            [pine.eval :as eval]
            [pine.data-types :as dt]))

(defn- generate
  "Helper function to generate the sql"
  [expression]
  (-> expression
      parser/parse
      :result
      (ast/generate :test)
      eval/build-query))

(deftest test-build--query

  (testing "qualify table"
    (is (= "\"x\"" (eval/q "x")))
    (is (= "\"x\".\"y\"" (eval/q "x" "y"))))

  (testing "No expression"
    (is (= {:query "",
            :params nil}
           (generate ""))))

  (testing "Select"
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" LIMIT 250",
            :params nil}
           (generate "company"))))

  (testing "Count"
    (is (= {:query "WITH x AS ( SELECT \"c_0\".* FROM \"company\" AS \"c_0\" ) SELECT COUNT(*) FROM x",
            :params nil}
           (generate "company | count:")))
    (is (= {:query "WITH x AS ( SELECT \"c_0\".* FROM \"company\" AS \"c_0\" LIMIT 100 ) SELECT COUNT(*) FROM x",
            :params nil}
           (generate "company | limit: 100 | count:"))))

  (testing "Condition : ="
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" = ? LIMIT 250",
            :params (map dt/string ["Acme Inc."])}
           (generate "company | where: name='Acme Inc.'")))
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" LIKE ? AND \"c_0\".\"country\" = ? LIMIT 250",
            :params (map dt/string ["Acme%", "PK"])}
           (generate "company | where: name like 'Acme%' | country = 'PK'"))))

  (testing "Condition : !="
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" != ? LIMIT 250",
            :params (map dt/string ["Acme Inc."])}
           (generate "company | where: name != 'Acme Inc.'"))))

  (testing "Condition : IN"
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"country\" IN (?, ?) LIMIT 250",
            :params (map dt/string ["PK", "DK"])}
           (generate "company | where: country in ('PK' 'DK')"))))

  (testing "Condition : columns"
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" = \"country\" LIMIT 250",
            :params (map dt/string [])}
           (generate "company | where: name = country")))
    (is (= {:query "SELECT \"c\".* FROM \"company\" AS \"c\" WHERE \"c\".\"name\" != \"c\".\"country\" LIMIT 250",
            :params (map dt/string [])}
           (generate "company as c | name != c.country")))
    (is (= {:query "SELECT \"c\".* FROM \"company\" AS \"c\" WHERE \"c\".\"name\" != \"c\".\"country\" LIMIT 250",
            :params (map dt/string [])}
           (generate "company as c | c.name != c.country"))))

  (testing "Condition : NULL"
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"deleted_at\" IS NULL LIMIT 250",
            :params []}
           (generate "company | where: deleted_at is null")))
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"deleted_at\" IS NOT NULL LIMIT 250",
            :params []}
           (generate "company | where: deleted_at is not null")))
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"deleted_at\" IS NULL LIMIT 250",
            :params []}
           (generate "company | where: deleted_at = null"))))

  (testing "Joins"
    (is (= {:query "SELECT \"e_1\".* FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company | employee")))
    (is (= {:query "SELECT \"e_1\".* FROM \"y\".\"company\" AS \"c_0\" JOIN \"x\".\"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "y.company | x.employee")))
    (is (= {:query "SELECT \"c_1\".* FROM \"y\".\"employee\" AS \"e_0\" JOIN \"x\".\"company\" AS \"c_1\" ON \"e_0\".\"company_id\" = \"c_1\".\"id\" LIMIT 250",
            :params nil}
           (generate "y.employee | x.company")))
    (is (= {:query "SELECT \"d_2\".* FROM \"x\".\"company\" AS \"c_0\" JOIN \"y\".\"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" JOIN \"z\".\"document\" AS \"d_2\" ON \"e_1\".\"id\" = \"d_2\".\"employee_id\" LIMIT 250",
            :params nil}
           (generate "x.company | y.employee | z.document"))))

  (testing "Joins with a context"
    (is (= {:query "SELECT \"d_2\".* FROM \"x\".\"company\" AS \"c\" JOIN \"y\".\"employee\" AS \"e_1\" ON \"c\".\"id\" = \"e_1\".\"company_id\" JOIN \"z\".\"document\" AS \"d_2\" ON \"c\".\"id\" = \"d_2\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "x.company as c | y.employee | from: c | z.document"))))

  (testing "order"
    (is (= {:query "SELECT \"c_0\".* FROM \"company\" AS \"c_0\" ORDER BY \"c_0\".\"country\" DESC LIMIT 250",
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
    (is (= {:query "SELECT \"c_0\".\"id\", \"e_1\".* FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company | select: id | employee")))
    (is (= {:query "SELECT \"c_0\".\"id\", \"e_1\".\"id\" FROM \"company\" AS \"c_0\" JOIN \"employee\" AS \"e_1\" ON \"c_0\".\"id\" = \"e_1\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company | s: id | employee | s: id")))
    (is (= {:query "SELECT \"c\".\"id\", \"e\".* FROM \"company\" AS \"c\" JOIN \"employee\" AS \"e\" ON \"c\".\"id\" = \"e\".\"company_id\" LIMIT 250",
            :params nil}
           (generate "company as c | employee as e | s: c.id, e.*"))))

  (testing "group"
    (is (= {:query "SELECT \"e_0\".\"status\", COUNT(1) FROM \"email\" AS \"e_0\" GROUP BY \"e_0\".\"status\"",
            :params nil}
           (generate "email | group: status => count"))))

  (testing "delete action"
    (is (= {:query "DELETE FROM \"company\" WHERE \"id\" IN ( SELECT \"c_0\".\"id\" FROM \"company\" AS \"c_0\" )",
            :params nil}
           (generate "company | delete! .id"))))

  (testing "delete"
    (is (= {:query " /* No SQL. Evaluate the pine expression for results */ "}
           (generate "company | delete:")))))

(deftest test-format--query
  (testing "string"
    (is (= "\nSELECT \"c_0\".* FROM \"company\" AS \"c_0\" WHERE \"c_0\".\"name\" = 'Acme Inc.' LIMIT 250;\n"
           (-> "company | where: name='Acme Inc.'" generate eval/formatted-query)))))