(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(deftest test-parser

  (testing "Parse `table` expressions"
    (is (= (parse-expression "user")             [{:type :table, :value {:table "user"}}]))
    (is (= (parse-expression "public.user")      [{:type :table, :value {:schema "public" :table "user"}}]))
    (is (= (parse-expression "user as u")        [{:type :table, :value {:schema nil      :table "user" :alias "u"}}]))
    (is (= (parse-expression "public.user as u") [{:type :table, :value {:schema "public" :table "user" :alias "u"}}])))

  (testing "Parse `select` expressions"
    (is (= (parse-expression "select: name") [{:type :select, :value ["name"]}]))
    (is (= (parse-expression "s: id, name") [{:type :select, :value ["id" "name"]}])))

  (testing "Parse `limit` expressions"
    (is (= (parse-expression "limit: 100") [{:type :limit, :value 100}]))
    (is (= (parse-expression "l: 10") [{:type :limit, :value 10}]))))
