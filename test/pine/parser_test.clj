(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(deftest test-parser

  (testing "Parse `table` expressions"
    (is (= [{:type :table, :value {:table ""}}]                                 (parse "")))
    (is (= [{:type :table, :value {:table "user"}}]                             (parse "user")))
    (is (= [{:type :table, :value {:table "user" :schema "public"}}]            (parse "public.user")))
    (is (= [{:type :table, :value {:table "user" :alias "u"}}]                  (parse "user as u")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :alias "u"}}] (parse "public.user as u")))
    (is (= [{:type :table, :value {:table "user" :join-column "id"}}]           (parse "user .id"))))

  (testing "Parse `select` expressions"
    (is (= (parse "select: name") [{:type :select, :value [{:column  "name"}]}]))
    (is (= (parse "s: id, name") [{:type :select, :value [{:column "id"} {:column  "name"}]}])))

  (testing "Parse `limit` expressions"
    (is (= (parse "limit: 100") [{:type :limit, :value 100}]))
    (is (= (parse "l: 10") [{:type :limit, :value 10}]))
    (is (= (parse "10") [{:type :limit, :value 10}])))

  (testing "Parse `where` expressions"
    (is (= (parse "where: name='John Doe'") [{:type :where, :value ["name" "=" "John Doe"]}]))
    (is (= (parse "name = 'John Doe'") [{:type :where, :value ["name" "=" "John Doe"]}]))
    (is (= (parse "name='John Doe'") [{:type :where, :value ["name" "=" "John Doe"]}]))
    (is (= (parse "name like 'John%'") [{:type :where, :value ["name" "like" "John%"]}]))
    (is (= (parse "age = 24") [{:type :where, :value ["age" "=" "24"]}])))

  (testing "Parse `delete` expressions"
    (is (= (parse "delete! .id") [{:type :delete, :value {:column "id"}}]))))
