(ns pine.core-test
  (:require [clojure.test :refer :all]
            [pine.core :refer :all]
            [pine.ast :as ast]
            [pine.fixtures :as fixtures]
            ))

(deftest pine-prepare:one-operation-no-filter
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "customers")
      ))))



(deftest pine-prepare:one-operation-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE (customers_0.id = ?) LIMIT 50"
       :params [[:number "1"]]
       }
      (pine-prepare fixtures/schema  "customers 1")
      ))))

(deftest pine-prepare:one-operation-explicit-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE (customers_0.id = ?) LIMIT 50"
       :params [[:string "1"]]
       }
      (pine-prepare fixtures/schema  "customers id=1")
      ))))

(deftest pine-prepare:one-operation-comprison-operator
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE (customers_0.id > ?) LIMIT 50"
       :params [[:string "1"]]
       }
      (pine-prepare fixtures/schema  "customers id>1")
      ))))

(deftest pine-prepare:one-operation-multiple-and-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE (customers_0.name LIKE ? AND customers_0.industry = ?) LIMIT 50"
       :params [[:string "acme%"], [:string "test"]]
       }
      (pine-prepare fixtures/schema  "customers name=acme* industry=test")
      ))))

(deftest pine-prepare:one-operation-multiple-or-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE (customers_0.name LIKE ? OR customers_0.industry = ?) LIMIT 50"
       :params [[:string "acme%"], [:string "test"]]
       }
      (pine-prepare fixtures/schema  "customers name=acme*, industry=test")
      ))))

(deftest pine-prepare:two-operation
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT caseFiles_1.* FROM customers AS customers_0 JOIN caseFiles AS caseFiles_1 ON (caseFiles_1.customerId = customers_0.id) WHERE (customers_0.name = ?) AND (caseFiles_1.title = ?) LIMIT 50"
       :params [[:string "Acme"] [:string "John"]]
       }
      (pine-prepare fixtures/schema "customers name=Acme | caseFiles title=John")
      ))))

(deftest pine-prepare:one-operation-without-filters
  (testing "Create sql a pine expression containing one operation without filters"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM customers AS customers_0 WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "customers")
      ))))

(deftest pine-prepare:two-operations-without-filters
  (testing "Create sql a pine expression containing two operations without filters"
    (is
     (=
      {
       :query "SELECT caseFiles_1.* FROM customers AS customers_0 JOIN caseFiles AS caseFiles_1 ON (caseFiles_1.customerId = customers_0.id) WHERE 1 AND 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "customers | caseFiles")
      ))))


(deftest pine-prepare:table-with-multiple-references
  (testing "Create sql from a pine expression that contains two tables. One contains multiple references to the other. Case File is related to the User through userId and createdByUserId. The first one should be picked up."
    (is
     (=
      {
       :query "SELECT caseFiles_1.* FROM users AS users_0 JOIN caseFiles AS caseFiles_1 ON (caseFiles_1.userId = users_0.id) WHERE 1 AND 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | caseFiles")
      ))))

(deftest pine-prepare:one-table-selected-columns
  (testing "Create sql from pine expressions for one entity selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.id, users_0.fullName AS name FROM users AS users_0 WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | select: id, fullName AS name")
      ))))

(deftest pine-prepare:two-tables-selected-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.id, users_0.fullName, caseFiles_2.id FROM users AS users_0 JOIN caseFiles AS caseFiles_2 ON (caseFiles_2.userId = users_0.id) WHERE 1 AND 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | select: id, fullName | caseFiles | select: id")
      ))))

(deftest pine-prepare:two-tables-selected-and-all-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.id, users_0.fullName, caseFiles_2.* FROM users AS users_0 JOIN caseFiles AS caseFiles_2 ON (caseFiles_2.userId = users_0.id) WHERE 1 AND 1 ORDER BY caseFiles_2.id DESC LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | select: id, fullName | caseFiles | o: id")
      ))))


(deftest pine-prepare:self-join
  (testing "Self join"
    (is
     (=
      {
       :query "SELECT folders_1.* FROM folders AS folders_0 JOIN folders AS folders_1 ON (folders_1.id = folders_0.parentId) WHERE (folders_0.id = ?) AND 1 LIMIT 50"
       :params [[:number "1"]]
       }
      (pine-prepare fixtures/schema "folders 1 | folders")
      ))))

(deftest pine-prepare:set-values
  (testing "Set values"
    (is
     (=
      {
       :query "UPDATE folders AS folders_0 SET folders_0.title = ? WHERE (folders_0.id = ?) LIMIT 50"
       :params [[:string "test"] [:number "1"]]
       }
      (pine-prepare fixtures/schema "folders 1 | set! title=test")
      ))))

(deftest pine-prepare:one-operation-group-implicit-fn
  (testing "Group by id and implicitly use the default group function i.e. count"
    (is
     (=
      {
       :query "SELECT customers_0.status, count(customers_0.status) FROM customers AS customers_0 WHERE 1 GROUP BY customers_0.status LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema  "customers | group: status")
      ))))

(deftest pine-prepare:one-operation-group-explicity-fn
  (testing "Group by id and specify a group function"
    (is
     (=
      {
       :query "SELECT customers_0.status, max(customers_0.id) FROM customers AS customers_0 WHERE 1 GROUP BY customers_0.status LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema  "customers | group: status max: id")
      ))))

(deftest pine-prepare:condition-in
  (testing "Specify multiple ids to select from"
    (is
     (=
      {
       :query "SELECT users_0.* FROM users AS users_0 WHERE (users_0.id IN ?) LIMIT 50"
       :params [[:expression "(1,2,3,4)"]]
       }
      (pine-prepare fixtures/schema  "users 1,2,3,4")
      ))))

(deftest pine-prepare:condition-is-not-null
  (testing "Check if the value is not null"
    (is
     (=
      {
       :query "SELECT users_0.* FROM users AS users_0 WHERE (users_0.name IS NOT NULL) LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema  "users name?")
      ))))

(deftest pine-prepare:condition-is-null
  (testing "Check if the value is null"
    (is
     (=
      {
       :query "SELECT users_0.* FROM users AS users_0 WHERE (users_0.name IS NULL) LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema  "users !name?")
      ))))

(deftest pine-prepare:function-count
  (testing "Count all the rows"
    (is
     (=
      {
       :query "SELECT count(users_0.id) FROM users AS users_0 WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema  "users | count: id")
      ))))
