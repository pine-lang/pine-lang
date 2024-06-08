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
           (generate :limit "l: 1")))))

