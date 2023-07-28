(ns pine.api-test
  (:require [clojure.test :refer :all]
            [pine.api :as api]

            [pine.db.connection-factory :as cf]))


(deftest build-query:single-resource-with-id
  (testing "Build a query using a single resource"
    (is
     (=
      "\nSELECT customers_0.* FROM `customers` AS customers_0 WHERE (customers_0.`id` = 1);\n"
      (api/build (cf/create :mysql) {:query "SELECT customers_0.* FROM `customers` AS customers_0 WHERE (customers_0.`id` = ?)"
              :params [[:number 1]]})
      ))))

(deftest build-query:single-resource-with-condition
  (testing "Build a query using a single resource"
    (is
     (=
      "\nSELECT customers_0.* FROM `customers` AS customers_0 WHERE (customers_0.`name` = \"Acme Inc\");\n"
      (api/build (cf/create :mysql) {:query "SELECT customers_0.* FROM `customers` AS customers_0 WHERE (customers_0.`name` = ?)"
              :params [[:string "Acme Inc"]]})
      ))))
