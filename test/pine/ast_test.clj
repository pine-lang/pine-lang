(ns pine.ast-test
  (:require [pine.ast :as ast]
            [clojure.test :refer :all]))


;; Parsing the Operations

(deftest str->operations:one-operation
  (testing "Create operations for a query"
    (is
     (=
      (ast/str->operations "customers 1")
      [{:entity :customers, :filter {:id "1"}}]
      ))))

(deftest str->operations:two-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/str->operations "customers 1 | users 2")
      [{:entity :customers, :filter {:id "1"}}
       {:entity :users, :filter {:id "2"}}]
      ))))

(deftest str->operations:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/str->operations "customers 1 | users 2 | address test")
      [{:entity :customers, :filter {:id "1"}}
       {:entity :users, :filter {:id "2"}}
       {:entity :address, :filter {:name "test"}}
       ]
      ))))

;; Opertations to AST

(deftest operations->join
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->join
       {:entity :customers, :filter {:id "1"}}
       {:entity :users, :filter {:id "2"}})
      [:users "u" ["u.customerId" "c.id"]]
      ))))

(deftest operations->joins:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       [{:entity :customers, :filter {:id "1"}}
        {:entity :users, :filter {:id "2"}}
        {:entity :address, :filter {:name "test"}}
        ])
      [:users "u" ["u.customerId" "c.id"]
       :address "a" ["a.userId" "u.id"]]
      ))))


;; AST to SQL

;; (deftest ast->sql:one-operation
;;   (testing "Create sql from an ast with one operation"
;;     (is
;;      (=
;;       (ast/ast->sql [{:entity :customers, :filter {:id "1"}}])
;;       "SELECT * FROM customers WHERE 1  AND id = 1"
;;       ))))


;; Utils

(deftest singular
  (testing "Make a word singular"
    (is
     (=
      (ast/singular "apples")
      "apple"
      ))))
