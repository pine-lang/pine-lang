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
       :query "SELECT customers.* FROM customers AS customers WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "customers")
      ))))



(deftest pine-prepare:one-operation-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers.* FROM customers AS customers WHERE customers.id = ? LIMIT 50"
       :params ["1"]
       }
      (pine-prepare fixtures/schema  "customers 1")
      ))))

(deftest pine-prepare:one-operation-multiple-and-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers.* FROM customers AS customers WHERE customers.name LIKE ? AND customers.industry = ? LIMIT 50"
       :params ["acme%", "test"]
       }
      (pine-prepare fixtures/schema  "customers name=acme* industry=test")
      ))))

(deftest pine-prepare:two-operation
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT caseFiles.* FROM customers AS customers JOIN caseFiles AS caseFiles ON (caseFiles.customerId = customers.id) WHERE customers.name = ? AND caseFiles.title = ? LIMIT 50"
       :params ["Acme" "John"]
       }
      (pine-prepare fixtures/schema "customers name=Acme | caseFiles title=John")
      ))))

(deftest pine-prepare:one-operation-without-filters
  (testing "Create sql a pine expression containing one operation without filters"
    (is
     (=
      {
       :query "SELECT customers.* FROM customers AS customers WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "customers")
      ))))

(deftest pine-prepare:two-operations-without-filters
  (testing "Create sql a pine expression containing two operations without filters"
    (is
     (=
      {
       :query "SELECT caseFiles.* FROM customers AS customers JOIN caseFiles AS caseFiles ON (caseFiles.customerId = customers.id) WHERE 1 AND 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "customers | caseFiles")
      ))))


(deftest pine-prepare:table-with-multiple-references
  (testing "Create sql from a pine expression that contains two tables. One contains multiple references to the other. Case File is related to the User through userId and createdByUserId. The first one should be picked up."
    (is
     (=
      {
       :query "SELECT caseFiles.* FROM users AS users JOIN caseFiles AS caseFiles ON (caseFiles.userId = users.id) WHERE 1 AND 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | caseFiles")
      ))))

(deftest pine-prepare:one-table-selected-columns
  (testing "Create sql from pine expressions for one entity selecting specific columns"
    (is
     (=
      {
       :query "SELECT users.id, users.fullName FROM users AS users WHERE 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | select: id, fullName")
      ))))

(deftest pine-prepare:two-tables-selected-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT users.id, users.fullName, caseFiles.id FROM users AS users JOIN caseFiles AS caseFiles ON (caseFiles.userId = users.id) WHERE 1 AND 1 LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | select: id, fullName | caseFiles | select: id")
      ))))

(deftest pine-prepare:two-tables-selected-and-all-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT users.id, users.fullName, caseFiles.* FROM users AS users JOIN caseFiles AS caseFiles ON (caseFiles.userId = users.id) WHERE 1 AND 1 ORDER BY caseFiles.id DESC LIMIT 50"
       :params []
       }
      (pine-prepare fixtures/schema "users | select: id, fullName | caseFiles | o: id")
      ))))
