(ns pine.ast-test
  (:require [clojure.test :refer [deftest is testing]]
            [pine.parser :as parser]
            [pine.ast.main :as ast]
            [pine.data-types :as dt]))

(defn- generate
  "Helper function to generate and get the relevant part in the ast"
  ([expression]
   (generate identity expression))
  ([type expression]
   (-> expression
       parser/parse
       :result
       (ast/generate :test)
       type)))

(defn- generate-joins [expression]
  (generate #(select-keys % [:join-map :joins]) expression))

(deftest test-ast

  (testing "Generate ast for `tables`"
    (is (= [{:schema nil :table "company" :alias "c" :parent nil :join-column nil}]
           (generate :tables "company as c")))
    (is (= [{:schema nil :table "user" :alias "u_0" :parent nil  :join-column nil}]
           (generate :tables "user")))
    (is (= [{:schema "public" :table "user" :alias "u_0" :parent nil  :join-column nil}]
           (generate :tables "public.user")))
    (is (= [{:schema "public" :table "user" :alias "u_0" :parent true  :join-column nil}]
           (generate :tables "public.user^"))))

  (testing "Generate ast for `from`"
    (is (= "c"
           (generate :context "company as c | user | from: c | ")))
    (is (= "c"
           (generate :context "company as c | user | from: c | 1"))))

  (testing "Generate ast for `select`"
    (is (= [{:alias "c_0" :column "id" :index 1}]
           (generate :columns "company | s: id")))

    (is (= [{:alias "u" :column "id" :index 1}]
           (generate :columns "company | s: u.id")))
    (is (= [{:alias "c_0" :column "id" :column-alias "c_id" :index 1}]
           (generate :columns "company | s: id as c_id")))
    (is (= [{:alias "c_0" :column "id" :index 1} {:alias "e_1" :column "id" :index 3}]
           (generate :columns "company | s: id | employee | s: id")))
    (is (= [{:alias "c" :column "id" :index 1}]
           (generate :columns "company as c | s: id")))
    (is (= []
           (generate :columns "user")))
    (is (= [{:alias "u" :column "" :symbol "*" :index 1}]
           (generate :columns "user as u | s: u.*"))))

  (testing "Generate ast for `order`"
    (is (= [{:alias "c_0" :column "country" :direction "DESC" :index 1}]
           (generate :order "company | o: country")))
    (is (= [{:alias "c_0" :column "country" :direction "DESC" :index 1}
            {:alias "c_0" :column "created_at" :direction "DESC" :index 1}]
           (generate :order "company | o: country, created_at"))))

  (testing "Generate ast for `limit`"
    (is (= 10
           (generate :limit "limit: 10")))
    (is (= 1
           (generate :limit "l: 1"))))

  (testing "Generate ast for `where`"
    (is (= [[nil "name" "=" (dt/string "Acme")]]
           (generate :where "name = 'Acme'")))
    (is (= [[nil "id" "=" (dt/number "1")]]
           (generate :where "id = 1")))
    (is (= [["c_0" "name" "=" (dt/string "Acme")]]
           (generate :where "company | name = 'Acme'")))
    (is (= [["c" "name" "=" (dt/string "Acme")]]
           (generate :where "company as c | name = 'Acme'")))
    (is (= [["c" "name" "=" (dt/string "Acme")] ["c" "country" "=" (dt/string "PK")]]
           (generate :where "company as c | name = 'Acme' | country = 'PK'")))
    (is (= [["c" "country" "IN" [(dt/string "PK") (dt/string "DK")]]]
           (generate :where "company as c | country in ('PK', 'DK')"))))

  (testing "Generate ast for `join` where there is no relation"
    (is (= {:join-map {"a_0" {"b_1" nil}} :joins [["a_0" "b_1" nil]]}
           (generate-joins "a | b")))
    (is (= {:join-map {"a_0" {"b_1" nil}} :joins [["a_0" "b_1" nil]]}
           (generate-joins "a | b .a_id"))))

  (testing "Generate ast for `join` where there is a relation"
    (is (= {:join-map {"c_0" {"e_1" ["c_0" "id" :has "e_1" "company_id"]}} :joins [["c_0" "e_1" ["c_0" "id" :has "e_1" "company_id"]]]}
           (generate-joins "company | employee")))
    (is (= {:join-map {"c_0" {"e_1" ["c_0" "id" :has "e_1" "company_id"]}} :joins [["c_0" "e_1" ["c_0" "id" :has "e_1" "company_id"]]]}
           (generate-joins "company | employee .company_id")))
    (is (= {:join-map {"c_0" {"e_1" ["c_0" nil :has "e_1" nil]}}, :joins [["c_0" "e_1" ["c_0" nil :has "e_1" nil]]]}
           (generate-joins "company | employee .employee_id"))) ;; trying with incorrect id
    )
  (testing "Generate ast for `join` where there is ambiguity"
    (is (= {:join-map {"e_0" {"d_1" ["e_0" "id" :has "d_1" "created_by"]}}, :joins [["e_0" "d_1" ["e_0" "id" :has "d_1" "created_by"]]]}
           (generate-joins "employee | document .created_by")))
    (is (= {:join-map {"e_0" {"d_1" ["e_0" "id" :has "d_1" "employee_id"]}}, :joins [["e_0" "d_1" ["e_0" "id" :has "d_1" "employee_id"]]]}
           (generate-joins "employee | document .employee_id"))))

  (testing "Generate ast for `join` using self join"
    ;; By default, we narrow the results
    ;; i.e. we join with the child
    (is (= {:join-map {"e_0" {"e_1" ["e_0" "id" :has "e_1" "reports_to"]}} :joins [["e_0" "e_1" ["e_0" "id" :has "e_1" "reports_to"]]]}
           (generate-joins "employee | employee")))
    (is (= {:join-map {"e_0" {"e_1" ["e_0" "id" :has "e_1" "reports_to"]}} :joins [["e_0" "e_1" ["e_0" "id" :has "e_1" "reports_to"]]]}
           (generate-joins "employee | employee .reports_to")))

    ;; However, we can exlicitly saw that the table is a parent using the `^` character
    (is (= {:join-map {"e_0" {"e_1" ["e_0" "reports_to" :of "e_1" "id"]}} :joins [["e_0" "e_1" ["e_0" "reports_to" :of "e_1" "id"]]]}
           (generate-joins "employee | employee^")))
    (is (= {:join-map {"e_0" {"e_1" ["e_0" "reports_to" :of "e_1" "id"]}} :joins [["e_0" "e_1" ["e_0" "reports_to" :of "e_1" "id"]]]}
           (generate-joins "employee | employee^ .reports_to"))))

  (testing "Generate ast for `count`"
    (is (= {:column "*"} (generate :count "company | count:"))))

  (testing "Generate ast for `delete`"
    (is (= {:column "id"} (generate :delete "company | delete! .id"))))

  (testing "Generate ast for `group`"
    (is (= {:columns {:column "status"} :functions ["count"]} (generate :group "company | group: status => count")))
    (is (= {:columns {:alias "e" :column "status"} :functions ["count"]} (generate :group "email as e | group: e.status => count")))
    )
  )
