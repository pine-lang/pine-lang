(ns pine.parser-test
  (:require [clojure.test :refer :all]
            [pine.parser :refer :all]))

(defn- p [e]
  (-> e parse-or-fail))

(deftest test-parser

  (testing "Parse `table` expressions"
    (is (= [{:type :table, :value {:table ""}}]                                 (p "")))
    (is (= [{:type :table, :value {:table "user"}}]                             (p " user")))
    (is (= [{:type :table, :value {:table "user" :schema "public"}}]            (p "public.user")))
    (is (= [{:type :table, :value {:table "user" :schema "public"}}]            (p "public.user")))
    (is (= [{:type :table, :value {:table "user" :alias "u"}}]                  (p "user as u")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :alias "u"}}] (p "public.user as u")))
    (is (= [{:type :table, :value {:table "user" :join-column "id"}}]           (p "user .id")))
    (is (= [{:type :table, :value {:schema "public"
                                   :table "user"
                                   :join-column "id"}}]                         (p "public.user .id"))))

  (testing "Parse `table` expressions with directionality"

    ;; table
    (is (= [{:type :table, :value {:table "user" :parent false}}]                 (p "has: user")))
    (is (= [{:type :table, :value {:table "user" :parent true}}]                  (p "of: user")))
    (is (= [{:type :table, :value {:table "user" :parent true}}]                  (p "user^")))

    ;; schema.table
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent false}}] (p "has: public.user")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent true}}]  (p "of: public.user")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent true}}]  (p "public.user^")))

    ;; schema.table alias
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent false :alias "u"}}] (p "has: public.user as u")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent true :alias "u"}}]  (p "of: public.user as u")))
    (is (= [{:type :table, :value {:table "user" :schema "public" :parent true :alias "u"}}]  (p "public.user^ as u")))

    ;; table .column
    (is (= [{:type :table, :value {:table "user" :parent false :join-column "other_id"}}]     (p "has: user .other_id")))
    (is (= [{:type :table, :value {:table "user" :parent true :join-column "other_id"}}]      (p "of: user .other_id")))
    (is (= [{:type :table, :value {:table "user" :parent true :join-column "other_id"}}]      (p "user^ .other_id")))

    ;; schema.table .column
    (is (= [{:type :table, :value {:schema "public" :table "user" :join-column "other_id" :parent false}}]           (p "has: public.user .other_id")))
    (is (= [{:type :table, :value {:schema "public" :table "user" :join-column "other_id" :parent true}}]            (p "of: public.user .other_id")))
    (is (= [{:type :table, :value {:schema "public" :table "user" :join-column "other_id" :parent true}}]            (p "public.user^ .other_id")))

    ;; schema.table alias .column
    (is (= [{:type :table, :value {:schema "public" :table "user" :alias "u" :join-column "other_id" :parent false}}] (p "has: public.user .other_id as u ")))
    (is (= [{:type :table, :value {:schema "public" :table "user" :alias "u" :join-column "other_id" :parent true}}]  (p "of: public.user .other_id as u ")))
    (is (= [{:type :table, :value {:schema "public" :table "user" :alias "u" :join-column "other_id" :parent true}}]  (p "public.user^ .other_id as u "))))


  (testing "Parse `from` expressions"
    (is (= [{:type :from, :value {:alias "u"}}] (p "from: u"))))


  (testing "Parse `select` expressions"
    (is (= [{:type :select, :value [{:column  "name"}]}]                               (p "select: name")))
    (is (= [{:type :select, :value [{:alias "u" :column  "name"}]}]                    (p "select: u.name")))
    (is (= [{:type :select, :value [{:column "id"} {:column  "name"}]}]                (p "s: id, name")))
    (is (= [{:type :select, :value [{:column-alias "t_id" :column "id"}]}]             (p "s: id as t_id")))
    (is (= [{:type :select, :value [{:alias "u" :column "id" :column-alias "t_id"}]}] (p "s: u.id as t_id"))))

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

  (testing "Parse `where` `in` expressions"
    (is (= [{:type :where, :value ["age" "in" ["24"]]}]        (p "age in ('24')")))
    (is (= [{:type :where, :value ["age" "in" ["24"]]}]        (p "age in ('24' ) ")))
    (is (= [{:type :where, :value ["age" "in" ["24" "36"]]}]   (p "age in (  '24' ,'36' )")))
    (is (= [{:type :where, :value ["age" "in" ["24" "36"]]}]   (p "age in ('24' '36')"))))

  (testing "Parse `delete` expressions"
    (is (= [{:type :delete, :value {:column "id"}}] (p "delete! .id")))))
