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
    (is (= [{:type :table, :value {:table "user" :join-column "id"}}]           (parse "user .id")))
    (is (= [{:type :table, :value {:schema "public"
                                   :table "user"
                                   :join-column "id"}}]                         (parse "public.user .id"))))

  (testing "Parse `select` expressions"
    (is (= [{:type :select, :value [{:column  "name"}]}]                (parse "select: name")))
    (is (= [{:type :select, :value [{:column "id"} {:column  "name"}]}] (parse "s: id, name"))))

  (testing "Parse `limit` expressions"
    (is (= [{:type :limit, :value 100}] (parse "limit: 100")))
    (is (= [{:type :limit, :value 10}]  (parse "l: 10")))
    (is (= [{:type :limit, :value 10}]  (parse "10"))))

  (testing "Parse `where` expressions"
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (parse "where: name='John Doe'")))
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (parse "name = 'John Doe'")))
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (parse "name='John Doe'")))
    (is (= [{:type :where, :value ["name" "like" "John%"]}] (parse "name like 'John%'")))
    (is (= [{:type :where, :value ["age" "=" "24"]}]        (parse "age = 24"))))

  (testing "Parse `delete` expressions"
    (is (= [{:type :delete, :value {:column "id"}}] (parse "delete! .id")))))
