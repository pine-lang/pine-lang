(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(deftest test-parser

  (testing "Parse `resource` expressions"
    (is (= (parse-expression "user") [{:type :resource, :entity "user"}]))
    (is (= (parse-expression "user_document") [{:type :resource, :entity "user_document"}])))

  (testing "Parse `select` expressions"
    (is (= (parse-expression "select: name") [{:type :select, :columns ["name"]}]))
    (is (= (parse-expression "s: id, name") [{:type :select, :columns ["id" "name"]}])))

  (testing "Parse `limit` expressions"
    (is (= (parse-expression "limit: 100") [{:type :limit, :limit 100}]))
    (is (= (parse-expression "l: 10") [{:type :limit, :limit 10}]))))
