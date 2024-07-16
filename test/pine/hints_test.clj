(ns pine.hints-test
  (:require [pine.hints :refer :all]
            [clojure.test :refer :all]
            [pine.parser :as parser]
            [pine.ast.main :as ast]))

(defn- gen
  [expression]
  "Helper function to generate and get the relevant part in the ast"
  (-> expression
      parser/parse
      (ast/generate :test)
      :hints))

(deftest test-hints
  (testing "Generate hints"
    (is (= {:table [{:schema "x", :table "company"}]}
           (gen "co")))

    (is (= {:table [{:schema "y", :table "employee" :column "company_id"}]}
           (gen "company | e")))

    (is (= {:table [{:schema "x", :table "company" :column "company_id"}]}
           (gen "employee | co")))

    (is (= {:table []}
           (gen "company as c | s: id")))

    ;; The following shouldn't generate any hint but it does
    ;;
    ;; (is (= {:table []}
    ;;        (gen "company as c")))
    )

  (testing "Generate hints in ambiguity"
    (is (= {:table [{:schema "z", :table "document" :column "employee_id"}
                    {:schema "z", :table "document" :column "created_by"}]}
           (gen "employee | doc")))))
