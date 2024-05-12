(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(deftest test-parser

  (testing "Parse `table` expressions"
    (is (= (parse-expression "user") [{:type :table, :value "user"}]))
    (is (= (parse-expression "user_document") [{:type :table, :value "user_document"}])))

  (testing "Parse `select` expressions"
    (is (= (parse-expression "select: name") [{:type :select, :value ["name"]}]))
    (is (= (parse-expression "s: id, name") [{:type :select, :value ["id" "name"]}])))

  (testing "Parse `limit` expressions"
    (is (= (parse-expression "limit: 100") [{:type :limit, :value 100}]))
    (is (= (parse-expression "l: 10") [{:type :limit, :value 10}]))))
