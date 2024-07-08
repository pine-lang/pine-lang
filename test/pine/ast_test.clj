(ns pine.ast-test
  (:require [clojure.test :refer :all]
            [clojure.test :refer :all]
            [pine.parser :as parser]
            [pine.ast.main :as ast]))

(defn- generate
  ([expression]
   (generate identity expression))
  ([type expression]
   "Helper function to generate and get the relevant part in the ast"
   (-> expression
       parser/parse
       (ast/generate :test)
       type)))

(deftest test-ast
  (testing "Generate ast for `select`"
    (is (= [{:table "company" :alias "c"}]
           (generate :tables "company as c | s: id")))
    (is (= [{:table "user" :alias "u_0"}]
           (generate :tables "user"))))

  (testing "Generate ast for `tables`"
    (is (= [{:table "company" :alias "c"}]
           (generate :tables "company as c")))
    (is (= [{:table "user" :alias "u_0"}]

           (generate :tables "user"))))
  (testing "Generate ast for `limit`"
    (is (= 10
           (generate :limit "limit: 10")))
    (is (= 1
           (generate :limit "l: 1"))))

  (testing "Generate ast for `where`"
    (is (= [[nil "name" "=" "Acme"]]
           (generate :where "name = 'Acme'")))
    (is (= [["c_0" "name" "=" "Acme"]]
           (generate :where "company | name = 'Acme'")))
    (is (= [["c" "name" "=" "Acme"]]
           (generate :where "company as c | name = 'Acme'")))
    (is (= [["c" "name" "=" "Acme"] ["c" "country" "=" "PK"]]
           (generate :where "company as c | name = 'Acme' | country = 'PK'"))))

  (testing "Generate ast for `join` where there is no relation"
    (is (= {"a_0" {"b_1" nil}}
           (generate :joins "a | b"))))

  (testing "Generate ast for `join` where there is a relation"
    (is (= {"c_0" {"e_1" ["e_1" "company_id" := "c_0" "id"]}}
           (generate :joins "company | employee"))))

  (testing "Generate ast for `delete`"
    (is (= {:column "id"} (generate :delete "company | delete! using id")))))


