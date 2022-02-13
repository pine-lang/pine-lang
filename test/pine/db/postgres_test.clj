(ns pine.db.postgres-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.postgres :as postgres]
            [pine.fixtures.postgres :as fixtures]
            [pine.db.protocol :as protocol]
            )
  (:import pine.db.postgres.PostgresConnection)
  )

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:attachment "attachment_id"
       :tenant "tenant_id"
       }
      (protocol/references (PostgresConnection. "dummy" nil) fixtures/schema "user")
      ))))

;; (deftest relation:test-schema-owns
;;   (testing "Get the references of a table"
;;     (is
;;      (=
;;       "attachment_id"
;;       (db/relation fixtures/schema :user :owns :attachment))
;;      )))

;; ;; (db/relation fixtures/schema  :owns :attachment)

;; (deftest relation:test-schema-owned-by
;;   (testing "Get the references of a table"
;;     (is
;;      (=
;;       "caseFileId"
;;       (db/relation fixtures/schema :documents :owned-by :caseFiles))
;;      )))
