(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(deftest test-parser

  (testing "Parse `table` expressions"
    (is (= (parse "user")             [{:type :table, :value {:table "user"}}]))
    (is (= (parse "public.user")      [{:type :table, :value {:table "user" :schema "public"}}]))
    (is (= (parse "user as u")        [{:type :table, :value {:table "user" :alias "u"}}]))
    (is (= (parse "public.user as u") [{:type :table, :value {:table "user" :schema "public" :alias "u"}}])))

  (testing "Parse `select` expressions"
    (is (= (parse "select: name") [{:type :select, :value [{:column  "name"}]}]))
    (is (= (parse "s: id, name") [{:type :select, :value [{:column "id"} {:column  "name"}]}])))

  (testing "Parse `limit` expressions"
    (is (= (parse "limit: 100") [{:type :limit, :value 100}]))
    (is (= (parse "l: 10") [{:type :limit, :value 10}])))

  (testing "Parse `where` expressions"
    (is (= (parse "where: name='John Doe'") [{:type :where, :value ["name" "=" "John Doe"]}])))

  (testing "Parse `delete` expressions"
    (is (= (parse "delete! using id") [{:type :delete, :value {:column "id"}}]))))
