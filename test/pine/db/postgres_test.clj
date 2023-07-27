(ns pine.db.postgres-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.postgres :as postgres]
            [pine.fixtures.postgres :as fixtures]
            [pine.db.connection :as connection]
            ))

(def dc "Dummy connection" (atom (pine.db.postgres.Postgres. "dummy" nil)))

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:attachment "attachment_id"
       :tenant "tenant_id"
       }
      (connection/references @dc fixtures/schema "user")
      ))))

;; (connection/references @dc fixtures/schema "user")
