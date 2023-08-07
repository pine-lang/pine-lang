(ns pine.db.postgres-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.postgres :as postgres]
            [pine.fixtures.postgres :as fixtures]
            [pine.db.connection :as connection]
            [pine.db.connection-factory :as cf]
            [pine.config :as config]))

(deftest get-metadata:index-relations
  (testing "Get the references of a table"
    (is
     (=
      {:table
       {"user" {:in {"x"
                     {:refers-to {"organization" {:in {"public" {:via {"org_id" ["public" "organization" "id"]}}}}},
                      :referred-by {"document" {:in {"y" {:via {"user_id" ["x" "user" "id"]}}}}}}}},
        "organization" {:in {"public"
                             {:referred-by {"user" {:in {"x" {:via {"org_id" ["public" "organization" "id"]}}}}}}}},
        "document" {:in {"y"
                         {:refers-to {"user" {:in {"x" {:via {"user_id" ["x" "user" "id"]}}}}}}}}},
       :schema
       {"x" {:has {"user"
          {:refers-to {"organization" {:in {"public" {:via {"org_id" ["public" "organization" "id"]}}}}},
           :referred-by {"document" {:in {"y" {:via {"user_id" ["x" "user" "id"]}}}}}}}},
        "public" {:has {"organization"
          {:referred-by {"user" {:in {"x" {:via {"org_id" ["public" "organization" "id"]}}}}}}}},
        "y" {:has {"document"
          {:refers-to {"user" {:in {"x" {:via {"user_id" ["x" "user" "id"]}}}}}}}}}}
      (postgres/index-references fixtures/relations)))))

;; DEPRECATED
;; The following show be removed once we start using `get-metadata` on the `connection` protocol

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:attachment "attachment_id"
       :tenant "tenant_id"
       }
      (connection/get-references (cf/create :postgres) "user")
      ))))

;; (deftest get-columns:test-schema
;;   (testing "Get the columns of a table"
;;     (is
;;      (=
;;       ["id" "email" "status" "password" "salt" "attachment_id" "user_type" "sessionId" "tenant_id"]
;;       (connection/get-columns (cf/create :postgres) "user")
;;       ))))
