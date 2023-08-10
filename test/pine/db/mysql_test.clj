(ns pine.db.mysql-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.mysql :as mysql]
            [pine.fixtures.mysql :as fixtures]
            [pine.db.connection :as connection]

            [pine.config :as config]
            [pine.db.connection-factory :as cf]))

(deftest get-columns:test-schema
  (testing "Get the columns of a table"
    (is
     (=
      [ "id" "title" "customerId" "createdByUserId" ]
      (connection/get-columns (cf/create :mysql) "caseFiles")
      ))))
