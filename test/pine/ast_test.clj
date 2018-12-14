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
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :filters [[ "id" "1" ]]}]
      (ast/str->operations "customers 1")
      ))))

(deftest str->operations:max-function
  (testing "Create operations for a max function"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :filters []}
       {:type    "function"
        :fn-name "max"
        :columns ["created"]}]
      (ast/str->operations "customers | max: created")
      ))))

(deftest str->operations:one-operation-explicit-id
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :filters [[ "id" "1" ]]}]
      (ast/str->operations "customers id=1")
      ))))

(deftest str->operations:one-operation-name-has-number-and-wildcard
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :filters [[ "name" "1*" ]]}]
      (ast/str->operations "customers name=1*")
      ))))

(deftest str->operations:two-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :filters [[ "id" "1" ]]}
       {:type    "condition"
        :entity  :users
        :alias   "users_1"
        :filters [[ "id" "2" ]]}]
      (ast/str->operations "customers 1 | users 2")
      ))))

(deftest str->operations:three-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :filters [[ "id" "1" ]]}
       {:type    "condition"
        :entity  :users
        :alias   "users_1"
        :filters [[ "name" "John" ]]}
       {:type    "condition"
        :entity  :address
        :alias   "address_2"
        :filters []}
       ]
      (ast/str->operations "customers 1 | users name=John | address")
      ))))

;; Opertations to AST

(deftest operations->function-columns
  (testing "Create a where condition from operations"
    (is
     (=
      ["max(customers_0.created)"]
      (ast/operations->function-columns
       [{:type "condition" :entity :customers :filters [] :alias "customers_0"}
        {:type "function" :fn-name "max" :columns ["created"]}]
       )
      ))))

(deftest operation->where:customer-id-is-1
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "customers_0.id = ?" :params ["1"] }
      (->> (ast/str->operations "customers 1")
           first
           (ast/operation->where fixtures/schema true)
           )
      ))))

(deftest operation->where:customer-name-is-acme
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "customers_0.name = ?" :params ["acme"] }
      (->> (ast/str->operations "customers name=acme")
           first
           (ast/operation->where fixtures/schema true))
      ))))

(deftest operation->where:customer-name-is-acme-something
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "customers_0.name LIKE ?" :params ["acme%"] }
      (->> (ast/str->operations "customers name=acme*")
           first
           (ast/operation->where fixtures/schema true))
      ))))

(deftest operations->where
  (testing "Create a where condition from operations"
    (is
     (=
      {
       :conditions ["customers_0.name = ?"
                    "users_1.id = ?"]
       :params ["acme"
                "1"]
       }
      (->> "customers name=acme | users 1"
           ast/str->operations
           (ast/operations->where fixtures/schema true)
           )
      ))))

(deftest operations->join:entity-owns-another-entity
  (testing "Create operations for a query"
    (is
     (=
      [:documents "d" ["d.caseFileId" "cf.id"]]
      (ast/operations->join
       fixtures/schema
       {:entity :caseFiles :filters [[ "id" "1" ]], :alias "cf"}
       {:entity :documents :filters [[ "id" "2" ]], :alias "d"})
      ))))

(deftest operations->join:entity-owned-by-another-entity
  (testing "Create operations for a query"
    (is
     (=
      [:caseFiles "cf" ["cf.id" "d.caseFileId"]]
      (ast/operations->join
       fixtures/schema
       {:entity :documents :filters [[ "id" "2" ]] :alias "d"}
       {:entity :caseFiles :filters [[ "id" "1" ]] :alias "cf"}
       )
      ))))

(deftest operations->joins:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       fixtures/schema
       [{:entity :customers :filters [[ "id" "1" ]]      :alias "c"}
        {:entity :caseFiles :filters [[ "id" "2" ]]      :alias "cf"}
        {:entity :documents :filters [[ "name" "test" ]] :alias "d"}
        ])
      [:caseFiles "cf" ["cf.customerId" "c.id"]
       :documents "d" ["d.caseFileId" "cf.id"]]
      ))))

(deftest operations->joins:no-filter
  (testing "Create operations for a query"
    (is
     (=
      [:caseFiles "cf" ["cf.customerId" "c.id"]]
      (ast/operations->joins
       fixtures/schema
       [{:type "condition" :entity :customers :filters [] :alias "c"}
        {:type "condition" :entity :caseFiles :filters [] :alias "cf"}
        ])
      ))))

(deftest operations->group:no-filter
  (testing "Operatations to the group sql"
    (is
     (=
      nil
      (ast/operations->group
       [{:type "condition" :entity :customers, :filters []}
        ])
      ))))

(deftest operations->ast
  (testing "Create operations for a query"
    (is
     (=
      {
       :select ["documents_2.*"]
       :from [:customers "customers_0"]
       :joins [:caseFiles "caseFiles_1" ["caseFiles_1.customerId" "customers_0.id"]
               :documents "documents_2" ["documents_2.caseFileId" "caseFiles_1.id"]]
       :where {
               :conditions ["customers_0.id = ?"
                            "caseFiles_1.name = ?"
                            "documents_2.name = ?"
                            ]
               :params     ["1" "john" "test"]
               }
       :order nil
       :group nil
       :limit "LIMIT 50"
       :meta nil
       :delete nil
       }
      (ast/operations->ast
       fixtures/schema
       (ast/str->operations "customers 1 | caseFiles name=john | documents name=test")
       )
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
