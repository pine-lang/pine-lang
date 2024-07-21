(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(defn- p [e]
  (-> e parse-or-fail))

(deftest test-parser

  (testing "Parse `from` expressions"
    (is (= [{:type :table, :value {:table ""}}]                                 (p "")))
    (is (= [{:type :table, :value {:table "user"}}]                             (p "user")))
    (is (= [{:type :table, :value {:table "user" :schema "public"}}]            (p "public.user")))
    (is (= [{:type :table, :value {:table "user" :schema "public"}}]            (p "public.user")))
    (is (= [{:type :table, :value {:table "user" :alias "u"}}]                  (p "user as u")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :alias "u"}}] (p "public.user as u")))
    (is (= [{:type :table, :value {:table "user" :join-column "id"}}]           (p "user .id")))
    (is (= [{:type :table, :value {:schema "public"
                                   :table "user"
                                   :join-column "id"}}]                         (p "public.user .id"))))

  (testing "Parse `from` expressions with directionality"
    (is (= [{:type :table, :value {:table "user" :parent true}}]                  (p "user^")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent true}}] (p "public.user^")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent true :alias "u"}}] (p "public.user^ as u")))
    (is (= [{:type :table, :value {:schema "public"
                                   :table "user"
                                   :parent true
                                   :join-column "other_id"}}]                         (p "public.user^ .other_id")))
    (is (= [{:type :table, :value {:schema "public"
                                   :table "user"
                                   :parent true
                                   :alias "u"
                                   :join-column "other_id"}}]                         (p "public.user^ .other_id as u "))))

  (testing "Parse `select` expressions"
    (is (= [{:type :select, :value [{:column  "name"}]}]                (p "select: name")))
    (is (= [{:type :select, :value [{:column "id"} {:column  "name"}]}] (p "s: id, name")))
    (is (= [{:type :select, :value [{:column-alias "t_id" :column "id"}]}] (p "s: id as t_id"))))

  (testing "Parse `limit` expressions"
    (is (= [{:type :limit, :value 100}] (p "limit: 100")))
    (is (= [{:type :limit, :value 10}]  (p "l: 10")))
    (is (= [{:type :limit, :value 10}]  (p "10"))))

  (testing "Parse `where` expressions"
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (p "where: name='John Doe'")))
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (p "w: name='John Doe'")))
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (p "name = 'John Doe'")))
    (is (= [{:type :where, :value ["name" "=" "John Doe"]}] (p "name='John Doe'")))
    (is (= [{:type :where, :value ["name" "like" "John%"]}] (p "name like 'John%'")))
    (is (= [{:type :where, :value ["age" "=" "24"]}]        (p "age = 24"))))

  (testing "Parse `delete` expressions"
    (is (= [{:type :delete, :value {:column "id"}}] (p "delete! .id")))))
