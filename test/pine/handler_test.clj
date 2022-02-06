(ns pine.handler-test
  (:require [clojure.test :refer :all]
            [pine.handler :refer :all]
            [clojure.test :as t]
            [pine.fixtures.mysql :as fixtures]
            ))


(deftest build-query:single-resource-with-id
  (testing "Build a query using a single resource"
    (is
     (=
      "\nSELECT customers_0.* FROM `customers` AS customers_0 WHERE (customers_0.`id` = 1) LIMIT 50;\n"
      (build-query fixtures/schema "customers 1")
      ))))

(deftest build-query:single-resource-with-condition
  (testing "Build a query using a single resource"
    (is
     (=
      "\nSELECT customers_0.* FROM `customers` AS customers_0 WHERE (customers_0.`name` = \"Acme Inc\") LIMIT 50;\n"
      (build-query fixtures/schema "customers name='Acme Inc'")
      ))))
