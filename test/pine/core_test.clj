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
       :query "SELECT c.* FROM customers AS c WHERE 1 LIMIT 10"
       :params []
       }
      (pine-prepare fixtures/schema "customers *")
      ))))



(deftest pine-prepare:one-operation-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT c.* FROM customers AS c WHERE c.id = ? LIMIT 10"
       :params ["1"]
       }
      (pine-prepare fixtures/schema  "customers 1")
      ))))

(deftest pine-prepare:two-operation
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT cf.* FROM customers AS c JOIN caseFiles AS cf ON (cf.customerId = c.id) WHERE c.name LIKE ? AND cf.name LIKE ? LIMIT 10"
       :params ["Acme%" "John%"]
       }
      (pine-prepare fixtures/schema "customers Acme | caseFiles John")
      ))))
