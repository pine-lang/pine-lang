(ns pine.ast-test
  (:require [clojure.test :refer :all]
            [pine.parser :as parser]
            [pine.ast.main :as ast]))

(defn- generate [type expression]
  "Helper function to generate and get the relevant part in the ast"
  (->> expression
       parser/parse
       ast/generate
       type))

(deftest test-ast
  (testing "Generate ast for `tables`"

    (is (= [{:table "tenant" :alias "t"}]
           (generate :tables "tenant as t")))

    (is (= [{:table "user" :alias "user_0"}]
           (generate :tables "user"))))

  (testing "Generate ast for `limit`"
    (is (= 10
           (generate :limit "limit: 10")))
    (is (= 1
           (generate :limit "l: 1"))))


  (testing "Generate ast for `join` where there is no relation"
    (is (= {"a_0" {"b_1" nil}}
           (generate :joins "a | b")))
    )

  (testing "Generate ast for `join` where there is a relation"
    (is (= {"company_0" {"employee_1" ["employee_1" "company_id" := "company_0" "id"]}}
           (generate :joins "company | employee")))
    )
  )

