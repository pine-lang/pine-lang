(ns pine.ast-test
  (:require [pine.ast :as ast]
            [clojure.test :refer :all]
            [clojure.string :as s]
            [pine.fixtures.mysql :as fixtures]
            [pine.db :as db]
            [pine.db.connection :as connection]

            [pine.db.connection-factory :as cf]))

;; Parsing the Pine Expressions and Lexemes

(deftest str->operations:one-operation-implicit-id
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  "customers"
        :partial ["customers" [:schema :table]]
        :alias   "customers_0"
        :values  [[ "id" [:number "1"] "="]]
        :context {:schema nil, :entity nil, :alias nil}
        }]
      (ast/str->operations (cf/create :postgres) "customers 1")
      ))))

(deftest str->operations:group-explicit-fn
  (testing "Create operations for a max function"
    (is
     (=
      [{:type    "condition"
        :entity  "customers"
        :partial ["customers" [:schema :table]]
        :alias   "customers_0"
        :values  []
        :context {:schema nil, :entity nil :alias nil}
        }
       {:type    "group"
        :column  "status"
        :fn-name "max"
        :fn-column "id"
        :context {:schema "public" :entity "customers" :alias "customers_0"}
        }]
      (ast/str->operations (cf/create :postgres) "customers | group: status max: id")
      ))))

(deftest str->operations:one-operation-explicit-id
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type    "condition"
        :entity  "customers"
        :partial ["customers" [:schema :table]]
        :alias   "customers_0"
        :values  [[ "id" [:number "1"]  "="]]
        :context {:schema nil :entity nil :alias nil}
        :or      false
        }
       ]
      (ast/str->operations (cf/create :postgres) "customers id=1")
      ))))

(deftest str->operations:one-operation-name-has-number-and-wildcard
  (testing "Create operations for a query explicitly specifying the id"
    (is
     (=
      [{:type    "condition"
        :entity  "customers"
        :partial ["customers" [:schema :table]]
        :alias   "customers_0"
        :values  [[ "name" [:string "1*"] "="]]
        :context {:schema nil :entity nil, :alias nil}
        :or      false
        }]
      (ast/str->operations (cf/create :postgres) "customers name='1*'")
      ))))

(deftest str->operations:two-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  "customers"
        :partial ["customers" [:schema :table]]
        :alias   "customers_0"
        :values  [[ "id" [:number "1"] "="]]
        :context {:schema nil :entity nil :alias nil}
        }
       {:type    "condition"
        :entity  "users"
        :partial ["users" [:schema :table]]
        :alias   "users_1"
        :values  [[ "id" [:number "2"] "="]]
        :context {:schema "public" :entity "customers", :alias "customers_0"}
        }]
      (ast/str->operations (cf/create :postgres) "customers 1 | users 2")
      ))))

(deftest str->operations:three-operations
  (testing "Create operations for a query"
    (is
     (=
      [{:type    "condition"
        :entity  "customers"
        :partial ["customers" [:schema :table]]
        :alias   "customers_0"
        :values  [[ "id" [:number "1"] "="]]
        :context {:schema nil :entity nil, :alias nil}
        }
       {:type    "condition"
        :entity  "users"
        :partial ["users" [:schema :table]]
        :alias   "users_1"
        :values  [[ "name" [:string "John"] "="]]
        :context {:schema "public" :entity "customers" :alias "customers_0"}
        :or      false
        }
       {:type    "condition"
        :entity  "address"
        :partial ["address" [:schema :table]]
        :alias   "address_2"
        :values  []
        :context {:schema "public" :entity "users", :alias "users_1"}
        }
       ]
      (ast/str->operations (cf/create :postgres) "customers 1 | users name='John' | address")
      ))))

;; Opertations to AST

(deftest operations->group-implicit-fn
  (testing "Create a where condition from operations"
    (is
     (=
      ["customers_0.id, count(customers_0.id)"]
      (->> "customers 1 | group: id"
           (ast/str->operations (cf/create :postgres))
       (filter (ast/operation-type? ["group"]))
       ast/operations->group-columns
       )
      ))))

(deftest operation->where:customer-id-is-1
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "(customers_0.\"id\" = ?)" :params [[:number "1"]] }
      (->> (ast/str->operations (cf/create :postgres) "customers 1")
           first
           (ast/operation->where (cf/create :postgres) true)
           )
      ))))

(deftest operation->where:customer-name-is-acme
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "(customers_0.\"name\" = ?)" :params [[:string "acme"]] }
      (->> (ast/str->operations (cf/create :postgres) "customers name='acme'")
           first
           (ast/operation->where (cf/create :postgres) true))
      ))))

(deftest operation->where:customer-name-is-acme-something
  (testing "Create a where condition from an operation"
    (is
     (=
      { :conditions "(customers_0.\"name\" LIKE ?)" :params [[:string "acme%"]] }
      (->> (ast/str->operations (cf/create :postgres) "customers name='acme*'")
           first
           (ast/operation->where (cf/create :postgres) true))
      ))))

(deftest operations->where
  (testing "Create a where condition from operations"
    (is
     (=
      {
       :conditions ["(customers_0.\"name\" = ?)"
                    "(users_1.\"id\" = ?)"]
       :params [[:string "acme"] [:number "1"]]
       }
      (->> "customers name='acme' | users 1"
           (ast/str->operations (cf/create :postgres))
           (ast/operations->where (cf/create :postgres) true)
           )
      ))))

(deftest operations->join:entity-owns-another-entity
  (testing "Create operations for a query"
    (is
     (=
      ["\"organization\"" "o" ["u.\"org_id\"" "o.\"id\""]]
      (ast/operations->join
       (cf/create :postgres)
       {:entity "user", :alias "u"}
       {:entity "organization", :alias "o"})
      ))))

(deftest operations->join:entity-owned-by-another-entity
  (testing "Create operations for a query"
    (is
     (=
      ["\"user\"" "u" ["u.\"org_id\"" "o.\"id\""]]
      (ast/operations->join
       (cf/create :postgres)
       {:entity "organization", :alias "o"}
       {:entity "user", :alias "u"}
       )
      ))))

(deftest operations->joins:three-operations
  (testing "Create operations for a query"
    (is
     (=
      (ast/operations->joins
       (cf/create :postgres)
       [{:entity "organization" :alias "o"}
        {:entity "user" :alias "u"}
        {:entity "document" :alias "d"}
        ])

      ["\"user\"" "u" ["u.\"org_id\"" "o.\"id\""]
       "\"document\"" "d" ["d.\"user_id\"" "u.\"id\""]]
      ))))

(deftest operations->joins:no-filter
  (testing "Create operations for a query"
    (is
     (=
      ["\"user\"" "u" ["u.\"org_id\"" "o.\"id\""]]
      (ast/operations->joins
      (cf/create :postgres)
       [{:type "condition" :entity "organization" :values [] :alias "o"}
        {:type "condition" :entity "user" :values [] :alias "u"}
        ])
      ))))

(deftest operations->group:no-filter
  (testing "Operatations to the group sql"
    (is
     (=
      nil
      (ast/operations->group
       [{:type "condition" :entity "document", :values []}
        ])
      ))))

(deftest operations->ast
  (testing "Create operations for a query"
    (is
     (=
      {
       :select ["document_2.*"]
       :from [nil "organization" "organization_0"]
       :joins ["\"user\"" "user_1" ["user_1.\"org_id\"" "organization_0.\"id\""]
               "\"document\"" "document_2" ["document_2.\"user_id\"" "user_1.\"id\""]]
       :where {
               :conditions ["(organization_0.\"id\" = ?)"
                            "(user_1.\"name\" = ?)"
                            "(document_2.\"name\" = ?)"
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
      (->> "organization 1 | user name='john' | document name='test'"
           (ast/str->operations (cf/create :postgres))
           (ast/operations->ast (cf/create :postgres)))
      ))))

;; AST to SQL

(deftest ast->sql:one-operation
  (testing "Create sql from an ast with one operation"
    (is
     (=
      (ast/ast->sql-and-params (cf/create :postgres) {
                     :select ["c.*"]
                     :from [nil :organization "c"]
                     :where {
                             :conditions ["c.id = ?"
                                          ]
                             :params     ["1"]
                             }
                     })
      ["SELECT c.* FROM \"organization\" AS c WHERE c.id = ?" ["1"]]
      ))))

(deftest ast-join->sql
  (testing "Create sql from a single join from the joins part of the ast"
    (is
     (=
      (ast/ast-join->sql "\"users\"" "u" ["u.customerId" "c.id"])
      "JOIN \"users\" AS u ON (u.customerId = c.id)"
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
      (ast/ast-joins->sql ["\"users\"" "u" ["u.customerId" "c.id"]
                          "\"address\"" "a" ["a.userId" "u.id"]])
      "JOIN \"users\" AS u ON (u.customerId = c.id) JOIN \"address\" AS a ON (a.userId = u.id)"
      ))))

(deftest ast->sql-and-params:two-operation
  (testing "Create sql from an ast with two operations"
    (is
     (=
      (ast/ast->sql-and-params (cf/create :postgres) {
                                :select ["u.*"]
                                :from [nil :organization "c"]
                                :joins ["\"users\"" "u" ["u.customerId" "c.id"]]
                                :where {
                                        :conditions ["c.id = ?"
                                                     "u.name = ?"
                                                     ]
                                        :params     ["1" "john"]
                                        }
                     })
      ["SELECT u.* FROM \"organization\" AS c JOIN \"users\" AS u ON (u.customerId = c.id) WHERE c.id = ? AND u.name = ?" ["1" "john"]]
      ))))

(deftest str->operations:one-operation-comparison-less-than
  (testing "Create operations for a query explicitly specifying the id with a comparison operator"
    (is
     (=
      [{:type    "condition"
        :entity  "organization"
        :partial ["organization" [:schema :table]]
        :alias   "organization_0"
        :values  [[ "id" [:number "1"]  "<"]]
        :context {:schema nil :entity nil, :alias nil}
        :or      false
        }
       ]
      (ast/str->operations (cf/create :postgres) "organization id<1")
      ))))

(deftest str->operations:one-operation-comparison-greater-than
  (testing "Create operations for a query explicitly specifying the id with a comparison operator"
    (is
     (=
      [{:type    "condition"
        :entity  "organization"
        :partial ["organization" [:schema :table]]
        :alias   "organization_0"
        :values  [[ "id" [:number "1"]  ">"]]
        :context {:schema nil :entity nil, :alias nil}
        :or      false
        }
       ]
      (ast/str->operations (cf/create :postgres) "organization id>1")
      ))))
