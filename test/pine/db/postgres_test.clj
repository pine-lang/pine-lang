(ns pine.db.postgres-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.postgres :as postgres]
            [pine.fixtures.postgres :as fixtures]
            [pine.db.protocol :as protocol]
            )
  (:import pine.db.postgres.PostgresConnection)
  )

(def dc "Dummy connection" (atom (PostgresConnection. "dummy" nil)))

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:attachment "attachment_id"
       :tenant "tenant_id"
       }
      (protocol/references @dc fixtures/schema "user")
      ))))

;; (protocol/references @dc fixtures/schema "user")
