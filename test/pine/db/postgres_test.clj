(ns pine.db.postgres-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.postgres :as postgres]
            [pine.fixtures.postgres :as fixtures]
            [pine.db.connection :as connection]
            [pine.db.connection-factory :as cf]
            [pine.config :as config]))

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:attachment "attachment_id"
       :tenant "tenant_id"
       }
      (connection/references (cf/create :postgres) "user")
      ))))


(deftest get-columns:test-schema
  (testing "Get the columns of a table"
    (is
     (=
      ["id" "email" "status" "password" "salt" "attachment_id" "user_type" "sessionId" "tenant_id"]
      (connection/get-columns (cf/create :postgres) "user")
      ))))
