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

  (testing "Generate ast for `tables`"
    (is (= [{:table "company" :alias "c"}]
           (generate :tables "company as c")))
    (is (= [{:table "user" :alias "u_0"}]
           (generate :tables "user"))))

  (testing "Generate ast for `select`"
    (is (= [{:alias "c_0" :column "id"}]
           (generate :columns "company | s: id")))
    (is (= [{:alias "c_0" :column "id" :column-alias "c_id"}]
           (generate :columns "company | s: id as c_id")))
    (is (= [{:alias "c_0" :column "id"} {:alias "e_1" :column "id"}]
           (generate :columns "company | s: id | employee | s: id")))
    (is (= [{:alias "c" :column "id"}]
           (generate :columns "company as c | s: id")))
    (is (= []
           (generate :columns "user"))))

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
           (generate :joins "a | b")))
    (is (= {"a_0" {"b_1" nil}}
           (generate :joins "a | b .a_id"))))

  (testing "Generate ast for `join` where there is a relation"
    (is (= {"c_0" {"e_1" ["e_1" "company_id" := "c_0" "id"]}}
           (generate :joins "company | employee")))
    (is (= {"c_0" {"e_1" ["e_1" "company_id" := "c_0" "id"]}}
           (generate :joins "company | employee .company_id")))
    (is (= {"c_0" {"e_1" ["e_1" nil := "c_0" nil]}}
           (generate :joins "company | employee .employee_id"))) ;; trying with incorrect id
    )

  (testing "Generate ast for `delete`"
    (is (= {:column "id"} (generate :delete "company | delete! .id")))))


