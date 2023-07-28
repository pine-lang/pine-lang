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
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :mysql) "customers")
      ))))



(deftest pine-prepare:one-operation-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE (customers_0.`id` = ?)"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :mysql)  "customers 1")
      ))))

(deftest pine-prepare:one-operation-explicit-filter-on-id
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE (customers_0.`id` = ?)"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :mysql)  "customers id=1")
      ))))

(deftest pine-prepare:one-operation-comprison-operator
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE (customers_0.`id` > ?)"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :mysql)  "customers id>1")
      ))))

(deftest pine-prepare:one-operation-multiple-and-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE (customers_0.`name` LIKE ? AND customers_0.`industry` = ?)"
       :params [[:string "acme%"], [:string "test"]]
       }
      (pine-prepare (cf/create :mysql)  "customers name='acme*' industry='test'")
      ))))

(deftest pine-prepare:one-operation-multiple-or-filters
  (testing "Create sql a pine expression containing one operation with AND filters"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE (customers_0.`name` LIKE ? OR customers_0.`industry` = ?)"
       :params [[:string "acme%"], [:string "test"]]
       }
      (pine-prepare (cf/create :mysql)  "customers name='acme*', industry='test'")
      ))))

(deftest pine-prepare:two-operation
  (testing "Create sql a pine expression containing one operation"
    (is
     (=
      {
       :query "SELECT casefiles_1.`id`, casefiles_1.`title`, casefiles_1.`customerId`, casefiles_1.`createdByUserId` FROM `customers` AS customers_0 JOIN `caseFiles` AS casefiles_1 ON (casefiles_1.`customerId` = customers_0.`id`) WHERE (customers_0.`name` = ?) AND (casefiles_1.`title` = ?)"
       :params [[:string "Acme"] [:string "John"]]
       }
      (pine-prepare (cf/create :mysql) "customers name='Acme' | caseFiles title='John'")
      ))))

(deftest pine-prepare:one-operation-without-filters
  (testing "Create sql a pine expression containing one operation without filters"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :mysql) "customers")
      ))))

(deftest pine-prepare:two-operations-without-filters
  (testing "Create sql a pine expression containing two operations without filters"
    (is
     (=
      {
       :query "SELECT casefiles_1.`id`, casefiles_1.`title`, casefiles_1.`customerId`, casefiles_1.`createdByUserId` FROM `customers` AS customers_0 JOIN `caseFiles` AS casefiles_1 ON (casefiles_1.`customerId` = customers_0.`id`) WHERE true AND true"
       :params []
       }
      (pine-prepare (cf/create :mysql) "customers | caseFiles")
      ))))


(deftest pine-prepare:table-with-multiple-references
  (testing "Create sql from a pine expression that contains two tables. One contains multiple references to the other. Case File is related to the User through userId and createdByUserId. The first one should be picked up."
    (is
     (=
      {
       :query "SELECT casefiles_1.`id`, casefiles_1.`title`, casefiles_1.`customerId`, casefiles_1.`createdByUserId` FROM `users` AS users_0 JOIN `caseFiles` AS casefiles_1 ON (casefiles_1.`userId` = users_0.`id`) WHERE true AND true"
       :params []
       }
      (pine-prepare (cf/create :mysql) "users | caseFiles")
      ))))

(deftest pine-prepare:one-table-selected-columns
  (testing "Create sql from pine expressions for one entity selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName` AS name FROM `users` AS users_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :mysql) "users | select: id, fullName as name")
      ))))

(deftest pine-prepare:two-tables-selected-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName`, casefiles_2.`id` FROM `users` AS users_0 JOIN `caseFiles` AS casefiles_2 ON (casefiles_2.`userId` = users_0.`id`) WHERE true AND true"
       :params []
       }
      (pine-prepare (cf/create :mysql) "users | select: id, fullName | caseFiles | select: id")
      ))))

(deftest pine-prepare:two-tables-selected-and-all-columns
  (testing "Create sql from pine expressions for two tables selecting specific columns"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName`, casefiles_2.`id`, casefiles_2.`title`, casefiles_2.`customerId`, casefiles_2.`createdByUserId` FROM `users` AS users_0 JOIN `caseFiles` AS casefiles_2 ON (casefiles_2.`userId` = users_0.`id`) WHERE true AND true ORDER BY casefiles_2.`id` DESC"
       :params []
       }
      (pine-prepare (cf/create :mysql) "users | select: id, fullName | caseFiles | o: id")
      ))))


(deftest pine-prepare:self-join
  (testing "Self join"
    (is
     (=
      {:query
        "SELECT folders_1.`id`, folders_1.`title`, folders_1.`parentId` FROM `folders` AS folders_0 JOIN `folders` AS folders_1 ON (folders_1.`id` = folders_0.`parentId`) WHERE (folders_0.`id` = ?) AND true"
       :params [[:number "1"]]
       }
      (pine-prepare (cf/create :mysql) "folders 1 | folders")
      ))))

(deftest pine-prepare:set-value-string
  (testing "Set string values"
    (is
     (=
      {
       :query "UPDATE `folders` AS folders_0 SET `title` = ? WHERE (folders_0.`id` = ?)"
       :params [[:string "test"] [:number "1"]]
       }
      (pine-prepare (cf/create :mysql) "folders 1 | set! title='test'")
      ))))

(deftest pine-prepare:set-value-number
  (testing "Set number values"
    (is
     (=
      {
       :query "UPDATE `folders` AS folders_0 SET `title` = ? WHERE (folders_0.`id` = ?)"
       :params [[:number "123"] [:number "1"]]
       }
      (pine-prepare (cf/create :mysql) "folders 1 | set! title=123")
      ))))

(deftest pine-prepare:set-values
  (testing "Set string values"
    (is
     (=
      {
       :query "UPDATE `folders` AS folders_0 SET `title` = ?, `order` = ? WHERE (folders_0.`id` = ?)"
       :params [[:string "test"] [:number "10"] [:number "1"]]
       }
      (pine-prepare (cf/create :mysql) "folders 1 | set! title='test' order=10")
      ))))

(deftest pine-prepare:one-operation-group-implicit-fn
  (testing "Group by id and implicitly use the default group function i.e. count"
    (is
     (=
      {
       :query "SELECT customers_0.status, count(customers_0.status) FROM `customers` AS customers_0 WHERE true GROUP BY customers_0.status"
       :params []
       }
      (pine-prepare (cf/create :mysql)  "customers | group: status")
      ))))

(deftest pine-prepare:one-operation-group-explicity-fn
  (testing "Group by id and specify a group function"
    (is
     (=
      {
       :query "SELECT customers_0.status, max(customers_0.id) FROM `customers` AS customers_0 WHERE true GROUP BY customers_0.status"
       :params []
       }
      (pine-prepare (cf/create :mysql)  "customers | group: status max: id")
      ))))

(deftest pine-prepare:condition-in
  (testing "Specify multiple ids to select from"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName`, users_0.`realEmail` FROM `users` AS users_0 WHERE (users_0.`id` IN ?)"
       :params [[:expression "(1,2,3,4)"]]
       }
      (pine-prepare (cf/create :mysql)  "users 1,2,3,4")
      ))))

(deftest pine-prepare:condition-value-in
  (testing "Specify multiple ids to select from"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName`, users_0.`realEmail` FROM `users` AS users_0 WHERE (users_0.`id` IN ?)"
       :params [[:expression "(1,2,3)"]]
       }
      (pine-prepare (cf/create :mysql)  "users id=1,2,3")
      ))))

(deftest pine-prepare:condition-is-not-null
  (testing "Check if the value is not null"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName`, users_0.`realEmail` FROM `users` AS users_0 WHERE (users_0.`name` IS NOT ?)"
       :params [[:expression "NULL"]]
       }
      (pine-prepare (cf/create :mysql)  "users name?")
      ))))

(deftest pine-prepare:condition-is-null
  (testing "Check if the value is null"
    (is
     (=
      {
       :query "SELECT users_0.`id`, users_0.`fullName`, users_0.`realEmail` FROM `users` AS users_0 WHERE (users_0.`name` IS ?)"
       :params [[:expression "NULL"]]
       }
      (pine-prepare (cf/create :mysql)  "users !name?")
      ))))

(deftest pine-prepare:function-count
  (testing "Count all the rows"
    (is
     (=
      {
       :query "SELECT count(users_0.id) FROM `users` AS users_0 WHERE true"
       :params []
       }
      (pine-prepare (cf/create :mysql)  "users | count: id")
      ))))

(deftest pine-prepare:condition-empty-string
  (testing "Create sql a pine expression containing one operation with a condition where the value is an empty string"
    (is
     (=
      {
       :query "SELECT customers_0.`id`, customers_0.`name` FROM `customers` AS customers_0 WHERE (customers_0.`industry` = ?)"
       :params [[:string ""]]
       }
      (pine-prepare (cf/create :mysql)  "customers industry=''")
      ))))
