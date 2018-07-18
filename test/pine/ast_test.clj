(ns pine.ast-test
  (:require [pine.ast :as ast]
            [clojure.test :refer :all]
            [clojure.string :as s]))

(def *test-schema* {})

;; Parsing the Pine Expressions and Lexemes

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
      (ast/str->operations "customers 1 | users John | address *")
      [{:entity :customers, :filter {:id "1"}}
       {:entity :users, :filter {:name "John"}}
       {:entity :address, :filter {}}
       ]
      ))))

;; Opertations to AST

(deftest operation->where:customer-id-is-1
  (testing "Create a where condition from an operation"
    (is
     (=
      (ast/operation->where
       *test-schema*
       {:entity :customers, :filter {:id "1"}})
      ["c.id = ?" "1"]
      ))))

(deftest operation->where:customer-name-is-acme
  (testing "Create a where condition from an operation"
    (is
     (=
      (ast/operation->where
       *test-schema*
       {:entity :customers, :filter {:name "acme"}})
      ["c.name LIKE ?" "acme%"]
      ))))

(deftest operations->where
  (testing "Create a where condition from operations"
    (is
     (=
      (ast/operations->where
       *test-schema*
       [
        {:entity :customers, :filter {:name "acme"}}
        {:entity :users    , :filter {:id "1"}}
        ]
       )
      {
       :conditions ["c.name LIKE ?"
                    "u.id = ?"]
       :params ["acme%"
                "1"]
       }
      ))))

(deftest operations->join
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->join
       *test-schema*
       {:entity :customers, :filter {:id "1"}}
       {:entity :users, :filter {:id "2"}})
      [:users "u" ["u.customerId" "c.id"]]
      ))))

(deftest operations->joins:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       *test-schema*
       [{:entity :customers, :filter {:id "1"}}
        {:entity :users, :filter {:id "2"}}
        {:entity :address, :filter {:name "test"}}
        ])
      [:users "u" ["u.customerId" "c.id"]
       :address "a" ["a.userId" "u.id"]]
      ))))

(deftest operations->ast
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->ast
       *test-schema*
       [{:entity :customers, :filter {:id "1"}}
        {:entity :users, :filter {:name "john"}}
        {:entity :address, :filter {:name "xyz"}}
        ])
      {
       :select ["a.*"]
       :from [:customers "c"]
       :joins [
              :users "u" ["u.customerId" "c.id"]
              :address "a" ["a.userId" "u.id"]
              ]
       :where {
               :conditions ["c.id = ?"
                            "u.name LIKE ?"
                            "a.name LIKE ?"
                            ]
               :params     ["1" "john%" "xyz%"]
               }
       }
      ))))

;; AST to SQL

(deftest ast->sql:one-operation
  (testing "Create sql from an ast with one operation"
    (is
     (=
      (ast/ast->sql {
                     :select ["c.*"]
                     :from [:customers "c"]
                     :where {
                             :conditions ["c.id = ?"
                                          ]
                             :params     ["1"]
                             }
                     })
      ["SELECT c.* FROM customers AS c WHERE c.id = ? LIMIT 10" ["1"]]
      ))))

(deftest ast-join->sql
  (testing "Create sql from a single join from the joins part of the ast"
    (is
     (=
      (ast/ast-join->sql :users "u" ["u.customerId" "c.id"])
      "JOIN users AS u ON (u.customerId = c.id)"
      ))))

(deftest ast-joins->sql:no-join
  (testing "Create sql from a joins part of the ast"
    (is
     (=
      (ast/ast-joins->sql [])
      ""
      ))))

(deftest ast-joins->sql
  (testing "Create sql from a joins part of the ast"
    (is
     (=
      (ast/ast-joins->sql [:users "u" ["u.customerId" "c.id"]
                          :address "a" ["a.userId" "u.id"]])
      "JOIN users AS u ON (u.customerId = c.id) JOIN address AS a ON (a.userId = u.id)"
      ))))

(deftest ast->sql-and-params:two-operation
  (testing "Create sql from an ast with two operations"
    (is
     (=
      (ast/ast->sql-and-params {
                     :select ["u.*"]
                     :from [:customers "c"]
                     :joins [:users "u" ["u.customerId" "c.id"]]
                     :where {
                             :conditions ["c.id = ?"
                                          "u.name = ?"
                                          ]
                             :params     ["1" "john?"]
                             }
                     })
      ["SELECT u.* FROM customers AS c JOIN users AS u ON (u.customerId = c.id) WHERE c.id = ? AND u.name = ? LIMIT 10" ["1" "john?"]]
      ))))

;; Utils

(deftest singular
  (testing "Make a word singular"
    (is
     (=
      (ast/singular "apples")
      "apple"
      ))))
