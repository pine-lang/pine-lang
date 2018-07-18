(ns pine.core-test
  (:require [clojure.test :refer :all]
            [pine.core :refer :all]
            [pine.ast :as ast]
            ))

(def *test-schema* {:caseFiles
                    {:primary-key "caseFileId"
                     :references
                     {:documents "documentId"
                      :users "userId"}}})

(deftest pine-prepare:one-operation-no-filter
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT c.* FROM customers AS c WHERE 1 LIMIT 10"
       :params []
       }
      (pine-prepare *test-schema* "customers *")
      ))))



(deftest pine-prepare:one-operation-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT c.* FROM customers AS c WHERE c.id = ? LIMIT 10"
       :params ["1"]
       }
      (pine-prepare *test-schema*  "customers 1")
      ))))

(deftest pine-prepare:two-operation
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT u.* FROM customers AS c JOIN users AS u ON (u.customerId = c.id) WHERE c.name LIKE ? AND u.name LIKE ? LIMIT 10"
       :params ["Acme%" "John%"]
       }
      (pine-prepare *test-schema* "customers Acme | users John")
      ))))
