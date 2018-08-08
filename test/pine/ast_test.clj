(ns pine.ast-test
  (:require [pine.ast :as ast]
            [clojure.test :refer :all]
            [clojure.string :as s]
            [pine.fixtures :as fixtures]
            ))

;; Parsing the Pine Expressions and Lexemes

(deftest str->operations:one-operation-implicit-id
  (testing "Create operations for a query"
    (is
     (=
      [{:type "condition" :entity :customers, :filters [[ "id" "1" ]]}]
      (ast/str->operations "customers 1")
      ))))

(deftest str->operations:one-operation-explicit-id
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type "condition" :entity :customers, :filters [[ "id" "1" ]]}]
      (ast/str->operations "customers id=1")
      ))))

(deftest str->operations:one-operation-name-has-number-and-wildcard
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type "condition" :entity :customers, :filters [[ "name" "1*" ]]}]
      (ast/str->operations "customers name=1*")
      ))))

(deftest str->operations:two-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type "condition" :entity :customers, :filters [[ "id" "1" ]]}
       {:type "condition" :entity :users, :filters [[ "id" "2" ]]}]
      (ast/str->operations "customers 1 | users 2")
      ))))

(deftest str->operations:three-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type "condition" :entity :customers, :filters [[ "id" "1" ]]}
       {:type "condition" :entity :users, :filters [[ "name" "John" ]]}
       {:type "condition" :entity :address, :filters []}
       ]
      (ast/str->operations "customers 1 | users name=John | address")
      ))))

;; Opertations to AST

(deftest operation->where:customer-id-is-1
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "c.id = ?" :params ["1"] }
      (ast/operation->where
       fixtures/schema
       {:entity :customers, :filters [[ "id" "1" ]]})
      ))))

(deftest operation->where:customer-name-is-acme
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "c.name = ?" :params ["acme"] }
      (ast/operation->where
       fixtures/schema
       {:entity :customers, :filters [[ "name" "acme" ]]})
      ))))

(deftest operation->where:customer-name-is-acme-something
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "c.name LIKE ?" :params ["acme%"] }
      (ast/operation->where
       fixtures/schema
       {:entity :customers, :filters [[ "name" "acme*" ]]})
      ))))

(deftest operations->where
  (testing "Create a where condition from operations"
    (is
     (=
      {
       :conditions ["c.name = ?"
                    "u.id = ?"]
       :params ["acme"
                "1"]
       }
      (ast/operations->where
       fixtures/schema
       [
        {:entity :customers, :filters [[ "name" "acme" ]]}
        {:entity :users    , :filters [[ "id" "1" ]]}
        ]
       )
      
      ))))

(deftest operations->join:entity-owns-another-entity
  (testing "Create operations for a query"
    (is
     (=
      [:documents "d" ["d.caseFileId" "cf.id"]]
      (ast/operations->join
       fixtures/schema
       {:entity :caseFiles, :filters [[ "id" "1" ]]}
       {:entity :documents, :filters [[ "id" "2" ]]})
      ))))

(deftest operations->join:entity-owned-by-another-entity
  (testing "Create operations for a query"
    (is
     (=
      [:caseFiles "cf" ["cf.id" "d.caseFileId"]]
      (ast/operations->join
       fixtures/schema
       {:entity :documents, :filters [[ "id" "2" ]]}
       {:entity :caseFiles, :filters [[ "id" "1" ]]}
       )
      ))))

(deftest operations->joins:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       fixtures/schema
       [{:entity :customers, :filters [[ "id" "1" ]]}
        {:entity :caseFiles, :filters [[ "id" "2" ]]}
        {:entity :documents, :filters [[ "name" "test" ]]}
        ])
      [:caseFiles "cf" ["cf.customerId" "c.id"]
       :documents "d" ["d.caseFileId" "cf.id"]]
      ))))

(deftest operations->joins:no-filter
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       fixtures/schema
       [{:type "condition" :entity :customers, :filters []}
        {:type "condition" :entity :caseFiles, :filters []}
        ])
      [:caseFiles "cf" ["cf.customerId" "c.id"]]
      ))))

(deftest operations->ast
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->ast
       fixtures/schema
       [{:type "condition" :entity :customers, :filters [[ "id" "1" ]]}
        {:type "condition" :entity :caseFiles, :filters [[ "name" "john" ]]}
        {:type "condition" :entity :documents, :filters [[ "name" "test" ]]}
        ])
      {
       :select ["d.*"]
       :from [:customers "c"]
       :joins [:caseFiles "cf" ["cf.customerId" "c.id"]
               :documents "d" ["d.caseFileId" "cf.id"]]
       :where {
               :conditions ["c.id = ?"
                            "cf.name = ?"
                            "d.name = ?"
                            ]
               :params     ["1" "john" "test"]
               }
       :limit "LIMIT 50"
       }
      ))))

;; AST to SQL

(deftest ast->sql:one-operation
  (testing "Create sql from an ast with one operation"
    (is
     (=
      (ast/ast->sql-and-params {
                     :select ["c.*"]
                     :from [:customers "c"]
                     :where {
                             :conditions ["c.id = ?"
                                          ]
                             :params     ["1"]
                             }
                     })
      ["SELECT c.* FROM customers AS c WHERE c.id = ?" ["1"]]
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
                             :params     ["1" "john"]
                             }
                     })
      ["SELECT u.* FROM customers AS c JOIN users AS u ON (u.customerId = c.id) WHERE c.id = ? AND u.name = ?" ["1" "john"]]
      ))))
