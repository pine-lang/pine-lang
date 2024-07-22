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
       :result
       (ast/generate :test)
       type)))

(deftest test-ast

  (testing "Generate ast for `tables`"
    (is (= [{:schema nil :table "company" :alias "c" :parent nil}]
           (generate :tables "company as c")))
    (is (= [{:schema nil :table "user" :alias "u_0" :parent nil}]
           (generate :tables "user")))
    (is (= [{:schema "public" :table "user" :alias "u_0" :parent nil}]
           (generate :tables "public.user")))
    (is (= [{:schema "public" :table "user" :alias "u_0" :parent true}]
           (generate :tables "public.user^"))))

  (testing "Generate ast for `select`"
    (is (= [{:alias "c_0" :column "id"}]
           (generate :columns "company | s: id")))

    (is (= [{:alias "u" :column "id"}]
           (generate :columns "company | s: u.id")))
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
    (is (= {"c_0" {"e_1" ["c_0" "id" :has "e_1" "company_id"]}}
           (generate :joins "company | employee")))
    (is (= {"c_0" {"e_1" ["c_0" "id" :has "e_1" "company_id"]}}
           (generate :joins "company | employee .company_id")))
    (is (= {"c_0" {"e_1" ["c_0" nil :has "e_1" nil]}}
           (generate :joins "company | employee .employee_id"))) ;; trying with incorrect id
    )
  (testing "Generate ast for `join` using self join"
    ;; By default, we narrow the results
    ;; i.e. we join with the child
    (is (= {"e_0" {"e_1" ["e_0" "id" :has "e_1" "reports_to"]}}
           (generate :joins "employee | employee")))
    (is (= {"e_0" {"e_1" ["e_0" "id" :has "e_1" "reports_to"]}}
           (generate :joins "employee | employee .reports_to")))

    ;; However, we can exlicitly saw that the table is a parent using the `^` character
    (is (= {"e_0" {"e_1" ["e_0" "reports_to" :of "e_1" "id"]}}
           (generate :joins "employee | employee^")))
    (is (= {"e_0" {"e_1" ["e_0" "reports_to" :of "e_1" "id"]}}
           (generate :joins "employee | employee^ .reports_to"))))

  (testing "Generate ast for `delete`"
    (is (= {:column "id"} (generate :delete "company | delete! .id")))))
