(ns pine.ast-test
  (:require [pine.ast :as ast]
            [clojure.test :refer :all]
            [clojure.string :as s]
            [pine.fixtures.mysql :as fixtures]
            ))

;; Parsing the Pine Expressions and Lexemes

(deftest str->operations:one-operation-implicit-id
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "id" [:number "1"] "="]]
        :context {:entity nil, :alias nil}
        }]
      (ast/str->operations "customers 1")
      ))))

(deftest str->operations:group-explicit-fn
  (testing "Create operations for a max function"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  []
        :context {:entity nil :alias nil}
        }
       {:type    "group"
        :column  "status"
        :fn-name "max"
        :fn-column "id"
        :context {:entity :customers :alias "customers_0"}
        }]
      (ast/str->operations "customers | group: status max: id")
      ))))

(deftest str->operations:one-operation-explicit-id
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "id" [:number "1"]  "="]]
        :context {:entity nil, :alias nil}
        :or      false
        }
       ]
      (ast/str->operations "customers id=1")
      ))))

(deftest str->operations:one-operation-name-has-number-and-wildcard
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "name" [:string "1*"] "="]]
        :context {:entity nil, :alias nil}
        :or      false
        }]
      (ast/str->operations "customers name='1*'")
      ))))

(deftest str->operations:two-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "id" [:number "1"] "="]]
        :context {:entity nil, :alias nil}
        }
       {:type    "condition"
        :entity  :users
        :alias   "users_1"
        :values  [[ "id" [:number "2"] "="]]
        :context {:entity :customers, :alias "customers_0"}
        }]
      (ast/str->operations "customers 1 | users 2")
      ))))

(deftest str->operations:three-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "id" [:number "1"] "="]]
        :context {:entity nil, :alias nil}
        }
       {:type    "condition"
        :entity  :users
        :alias   "users_1"
        :values  [[ "name" [:string "John"] "="]]
        :context {:entity :customers, :alias "customers_0"}
        :or      false
        }
       {:type    "condition"
        :entity  :address
        :alias   "address_2"
        :values  []
        :context {:entity :users, :alias "users_1"}
        }
       ]
      (ast/str->operations "customers 1 | users name='John' | address")
      ))))

;; Opertations to AST

(deftest operations->group-implicit-fn
  (testing "Create a where condition from operations"
    (is
     (=
      ["customers_0.id, count(customers_0.id)"]
      (->> "customers 1 | group: id"
       ast/str->operations
       (filter (ast/operation-type? ["group"]))
       ast/operations->group-columns
       )
      ))))

(deftest operation->where:customer-id-is-1
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "(customers_0.`id` = ?)" :params [[:number "1"]] }
      (->> (ast/str->operations "customers 1")
           first
           (ast/operation->where true)
           )
      ))))

;; (->>
;;  "customers 3 | set! id=1 name=1231"
;;  ast/str->operations
;;  (ast/operations->ast fixtures/schema)
;;  ast/ast->sql-and-params
;;  )

(deftest operation->where:customer-name-is-acme
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "(customers_0.`name` = ?)" :params [[:string "acme"]] }
      (->> (ast/str->operations "customers name='acme'")
           first
           (ast/operation->where true))
      ))))

(deftest operation->where:customer-name-is-acme-something
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "(customers_0.`name` LIKE ?)" :params [[:string "acme%"]] }
      (->> (ast/str->operations "customers name='acme*'")
           first
           (ast/operation->where true))
      ))))

(deftest operations->where
  (testing "Create a where condition from operations"
    (is
     (=
      {
       :conditions ["(customers_0.`name` = ?)"
                    "(users_1.`id` = ?)"]
       :params [[:string "acme"] [:number "1"]]
       }
      (->> "customers name='acme' | users 1"
           ast/str->operations
           (ast/operations->where true)
           )
      ))))

;; (deftest operations->exclude-columns
;;   (testing "Excluding columns"
;;     (is
;;       (=
;;        ["users_0.fullName" "users_0.realEmail"]
;;        (->> '({:type "unselect", :columns ["id"], :context {:entity :users, :alias "users_0"}})
;;             (ast/operation->exclude-columns fixtures/schema ["users_0.*"])
;;          )))))

(deftest operations->join:entity-owns-another-entity
  (testing "Create operations for a query"
    (is
     (=
      [:documents "d" ["d.`caseFileId`" "cf.`id`"]]
      (ast/operations->join
       fixtures/schema
       {:entity :caseFiles :values [[ "id" "1" ]], :alias "cf"}
       {:entity :documents :values [[ "id" "2" ]], :alias "d"})
      ))))

(deftest operations->join:entity-owned-by-another-entity
  (testing "Create operations for a query"
    (is
     (=
      [:caseFiles "cf" ["cf.`id`" "d.`caseFileId`"]]
      (ast/operations->join
       fixtures/schema
       {:entity :documents :values [[ "id" "2" ]] :alias "d"}
       {:entity :caseFiles :values [[ "id" "1" ]] :alias "cf"}
       )
      ))))

(deftest operations->joins:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       fixtures/schema
       [{:entity :customers :values [[ "id" "1" ]]      :alias "c"}
        {:entity :caseFiles :values [[ "id" "2" ]]      :alias "cf"}
        {:entity :documents :values [[ "name" "test" ]] :alias "d"}
        ])
      [:caseFiles "cf" ["cf.`customerId`" "c.`id`"]
       :documents "d" ["d.`caseFileId`" "cf.`id`"]]
      ))))

(deftest operations->joins:no-filter
  (testing "Create operations for a query"
    (is
     (=
      [:caseFiles "cf" ["cf.`customerId`" "c.`id`"]]
      (ast/operations->joins
       fixtures/schema
       [{:type "condition" :entity :customers :values [] :alias "c"}
        {:type "condition" :entity :caseFiles :values [] :alias "cf"}
        ])
      ))))

(deftest operations->group:no-filter
  (testing "Operatations to the group sql"
    (is
     (=
      nil
      (ast/operations->group
       [{:type "condition" :entity :customers, :values []}
        ])
      ))))

(deftest operations->ast
  (testing "Create operations for a query"
    (is
     (=
      {
       :select ["documents_2.*"]
       :from [:customers "customers_0"]
       :joins [:caseFiles "casefiles_1" ["casefiles_1.`customerId`" "customers_0.`id`"]
               :documents "documents_2" ["documents_2.`caseFileId`" "casefiles_1.`id`"]]
       :where {
               :conditions ["(customers_0.`id` = ?)"
                            "(casefiles_1.`name` = ?)"
                            "(documents_2.`name` = ?)"
                            ]
               :params     [[:number "1"] [:string "john"] [:string "test"]]
               }
       :order nil
       :group nil
       :limit nil
       :meta nil
       :delete nil
       :set nil
       }
      (->> "customers 1 | caseFiles name='john' | documents name='test'"
      ast/str->operations
      (ast/operations->ast fixtures/schema))
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
      ["SELECT c.* FROM `customers` AS c WHERE c.id = ?" ["1"]]
      ))))

(deftest ast-join->sql
  (testing "Create sql from a single join from the joins part of the ast"
    (is
     (=
      (ast/ast-join->sql :users "u" ["u.customerId" "c.id"])
      "JOIN `users` AS u ON (u.customerId = c.id)"
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
      "JOIN `users` AS u ON (u.customerId = c.id) JOIN `address` AS a ON (a.userId = u.id)"
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
      ["SELECT u.* FROM `customers` AS c JOIN `users` AS u ON (u.customerId = c.id) WHERE c.id = ? AND u.name = ?" ["1" "john"]]
      ))))

(deftest str->operations:one-operation-comparison-less-than
  (testing "Create operations for a query explicitly specifying the id with a comparison operator"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "id" [:number "1"]  "<"]]
        :context {:entity nil, :alias nil}
        :or      false
        }
       ]
      (ast/str->operations "customers id<1")
      ))))

(deftest str->operations:one-operation-comparison-greater-than
  (testing "Create operations for a query explicitly specifying the id with a comparison operator"
    (is
     (=
      [{:type    "condition"
        :entity  :customers
        :alias   "customers_0"
        :values  [[ "id" [:number "1"]  ">"]]
        :context {:entity nil, :alias nil}
        :or      false
        }
       ]
      (ast/str->operations "customers id>1")
      ))))

