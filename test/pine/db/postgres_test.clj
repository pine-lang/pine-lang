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
              {:refers-to {"organization" {:in {"public" {:via {"org_id" ["x" "user" "org_id" := "public" "organization" "id"]}}}}},
               :referred-by {"document" {:in {"y" {:via {"user_id" ["y" "document" "user_id" := "x" "user" "id"]}}}}}}},
             :refers-to {"organization" {:via {"org_id" [["x" "user" "org_id" := "public" "organization" "id"]]}}},
             :referred-by {"document" {:via {"user_id" [["y" "document" "user_id" := "x" "user" "id"]]}}}},
            "organization" {:in {"public"
              {:referred-by {"user" {:in {"x" {:via {"org_id" ["x" "user" "org_id" := "public" "organization" "id"]}}}}}}},
             :referred-by {"user" {:via {"org_id" [["x" "user" "org_id" := "public" "organization" "id"]]}}}},
            "document" {:in {"y" {:refers-to {"user" {:in {"x" {:via {"user_id" ["y" "document" "user_id" := "x" "user" "id"]}}}}}}},
             :refers-to {"user" {:via {"user_id" [["y" "document" "user_id" := "x" "user" "id"]]}}}}},
           :schema
           {"x" {:contains {"user" true}},
            "public" {:contains {"organization" true}},
            "y" {:contains {"document" true}}}}
      (postgres/index-references fixtures/relations)))))

