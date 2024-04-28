(ns pine.core-test
  (:require [clojure.test :refer :all]
            [pine.core :refer :all]
            [pine.ast :as ast]
            [pine.fixtures.mysql :as fixtures]

            [pine.db.connection-factory :as cf]))

(deftest pine-prepare:one-operation-no-filter
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :postgres) "customers")
      ))))



(deftest pine-prepare:one-operation-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE (customers_0.\"id\" = ?)"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :postgres)  "customers 1")
      ))))

(deftest pine-prepare:one-operation-explicit-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE (customers_0.\"id\" = ?)"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :postgres)  "customers id=1")
      ))))

(deftest pine-prepare:one-operation-comprison-operator
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE (customers_0.\"id\" > ?)"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :postgres)  "customers id>1")
      ))))

(deftest pine-prepare:one-operation-multiple-and-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE (customers_0.\"name\" LIKE ? AND customers_0.\"industry\" = ?)"
       :params [[:string "acme%"], [:string "test"]]
       }
      (pine-prepare (cf/create :postgres)  "customers name='acme*' industry='test'")
      ))))

(deftest pine-prepare:one-operation-multiple-or-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE (customers_0.\"name\" LIKE ? OR customers_0.\"industry\" = ?)"
       :params [[:string "acme%"], [:string "test"]]
       }
      (pine-prepare (cf/create :postgres)  "customers name='acme*', industry='test'")
      ))))

(deftest pine-prepare:two-operation
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT user_1.* FROM \"organization\" AS organization_0 JOIN \"user\" AS user_1 ON (user_1.\"org_id\" = organization_0.\"id\") WHERE (organization_0.\"name\" = ?) AND (user_1.\"title\" = ?)"
       :params [[:string "Acme"] [:string "John"]]
       }
      (pine-prepare (cf/create :postgres) "organization name='Acme' | user title='John'")
      ))))

(deftest pine-prepare:one-operation-without-filters
  (testing "Create sql a pine expression containing one operation without filters"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :postgres) "customers")
      ))))

(deftest pine-prepare:two-operations-without-filters
  (testing "Create sql a pine expression containing two operations without filters"
    (is
     (=
      {
       :query "SELECT user_1.* FROM \"organization\" AS organization_0 JOIN \"user\" AS user_1 ON (user_1.\"org_id\" = organization_0.\"id\") WHERE true AND true"
       :params []
       }
      (pine-prepare (cf/create :postgres) "organization | user")
      ))))


(deftest pine-prepare:one-table-selected-columns
  (testing "Create sql from pine expressions for one entity selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.\"id\", users_0.\"fullName\" AS name FROM \"users\" AS users_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :postgres) "users | select: id, fullName as name")
      ))))

(pine-prepare (cf/create :postgres) "user | select: id, title | document | select: id")
(deftest pine-prepare:two-tables-selected-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT user_0.\"id\", user_0.\"fullName\", document_2.\"id\" FROM \"user\" AS user_0 JOIN \"document\" AS document_2 ON (document_2.\"user_id\" = user_0.\"id\") WHERE true AND true"
       :params []
       }
      (pine-prepare (cf/create :postgres) "user | select: id, fullName | document | select: id")
      ))))

(deftest pine-prepare:two-tables-selected-and-all-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT user_0.\"id\", user_0.\"fullName\", document_2.* FROM \"user\" AS user_0 JOIN \"document\" AS document_2 ON (document_2.\"user_id\" = user_0.\"id\") WHERE true AND true ORDER BY document_2.\"id\" DESC"
       :params []
       }
      (pine-prepare (cf/create :postgres) "user | select: id, fullName | document | o: id")
      ))))


(deftest pine-prepare:set-value-string
  (testing "Set string values"
    (is
     (=
      {
       :query "UPDATE \"folders\" AS folders_0 SET \"title\" = ? WHERE (folders_0.\"id\" = ?)"
       :params [[:string "test"] [:number "1"]]
       }
      (pine-prepare (cf/create :postgres) "folders 1 | set! title='test'")
      ))))

(deftest pine-prepare:set-value-number
  (testing "Set number values"
    (is
     (=
      {
       :query "UPDATE \"folders\" AS folders_0 SET \"title\" = ? WHERE (folders_0.\"id\" = ?)"
       :params [[:number "123"] [:number "1"]]
       }
      (pine-prepare (cf/create :postgres) "folders 1 | set! title=123")
      ))))

(deftest pine-prepare:set-values
  (testing "Set string values"
    (is
     (=
      {
       :query "UPDATE \"folders\" AS folders_0 SET \"title\" = ?, \"order\" = ? WHERE (folders_0.\"id\" = ?)"
       :params [[:string "test"] [:number "10"] [:number "1"]]
       }
      (pine-prepare (cf/create :postgres) "folders 1 | set! title='test' order=10")
      ))))

(deftest pine-prepare:one-operation-group-implicit-fn
  (testing "Group by id and implicitly use the default group function i.e. count"
    (is
     (=
      {
       :query "SELECT customers_0.status, count(customers_0.status) FROM \"customers\" AS customers_0 WHERE true GROUP BY customers_0.status"
       :params []
       }
      (pine-prepare (cf/create :postgres)  "customers | group: status")
      ))))

(deftest pine-prepare:one-operation-group-explicity-fn
  (testing "Group by id and specify a group function"
    (is
     (=
      {
       :query "SELECT customers_0.status, max(customers_0.id) FROM \"customers\" AS customers_0 WHERE true GROUP BY customers_0.status"
       :params []
       }
      (pine-prepare (cf/create :postgres)  "customers | group: status max: id")
      ))))

(deftest pine-prepare:condition-in
  (testing "Specify multiple ids to select from"
    (is
     (=
      {
       :query "SELECT users_0.* FROM \"users\" AS users_0 WHERE (users_0.\"id\" IN ?)"
       :params [[:expression "(1,2,3,4)"]]
       }
      (pine-prepare (cf/create :postgres)  "users 1,2,3,4")
      ))))

(deftest pine-prepare:condition-value-in
  (testing "Specify multiple ids to select from"
    (is
     (=
      {
       :query "SELECT users_0.* FROM \"users\" AS users_0 WHERE (users_0.\"id\" IN ?)"
       :params [[:expression "(1,2,3)"]]
       }
      (pine-prepare (cf/create :postgres)  "users id=1,2,3")
      ))))

(deftest pine-prepare:condition-is-not-null
  (testing "Check if the value is not null"
    (is
     (=
      {
       :query "SELECT users_0.* FROM \"users\" AS users_0 WHERE (users_0.\"name\" IS NOT ?)"
       :params [[:expression "NULL"]]
       }
      (pine-prepare (cf/create :postgres)  "users name?")
      ))))

(deftest pine-prepare:condition-is-null
  (testing "Check if the value is null"
    (is
     (=
      {
       :query "SELECT users_0.* FROM \"users\" AS users_0 WHERE (users_0.\"name\" IS ?)"
       :params [[:expression "NULL"]]
       }
      (pine-prepare (cf/create :postgres)  "users !name?")
      ))))

(deftest pine-prepare:function-count
  (testing "Count all the rows"
    (is
     (=
      {
       :query "SELECT count(users_0.id) FROM \"users\" AS users_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :postgres)  "users | count: id")
      ))))

(deftest pine-prepare:condition-empty-string
  (testing "Create sql a pine expression containing one operation with a condition where the value is an empty string"
    (is
     (=
      {
       :query "SELECT customers_0.* FROM \"customers\" AS customers_0 WHERE (customers_0.\"industry\" = ?)"
       :params [[:string ""]]
       }
      (pine-prepare (cf/create :postgres)  "customers industry=''")
      ))))
