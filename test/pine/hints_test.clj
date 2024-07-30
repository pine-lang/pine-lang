(ns pine.hints-test
  (:require [pine.ast.hints :refer :all]
            [clojure.test :refer :all]
            [pine.parser :as parser]
            [pine.ast.main :as ast]))

(defn- gen
  [expression]
  "Helper function to generate and get the relevant part in the ast"
  (-> expression
      parser/parse
      :result
      (ast/generate :test)
      :hints))

(-> "company | e"
    parser/parse
    :result
    (ast/generate :test)
    ;; :hints
    )

(deftest test-hints
  (testing "Generate hints"
    (is (= {:table [{:schema "x", :table "company"
                     :pine "x.company"}]}
           (gen "co")))

    (is (= {:table [{:schema "y", :table "employee" :column "company_id" :parent false
                     :pine "y.employee .company_id"}]}
           (gen "company | e")))

    (is (= {:table [{:schema "x", :table "company" :column "company_id" :parent true
                     :pine "of: x.company .company_id"}]}
           (gen "employee | co")))

    (is (= {:table []}
           (gen "company as c | s: id")))

    ;; The following shouldn't generate any hint but it does
    ;;
    ;; (is (= {:table []}
    ;;        (gen "company as c")))
    )

  (testing "Generate hints in ambiguity"
    (is (= {:table
            [{:schema "z",
              :table "document"
              :column "employee_id"
              :parent false
              :pine "z.document .employee_id"}
             {:schema "z"
              :table "document"
              :column "created_by"
              :parent false
              :pine "z.document .created_by"}]}
           (gen "employee | doc"))))

  (testing "Generate hints when direction is specified"
    (is (= {:table
            [{:schema "y"
              :table "employee"
              :column "reports_to"
              :parent true
              :pine "of: y.employee .reports_to"}
             {:schema "y"
              :table "employee"
              :column "reports_to"
              :parent false
              :pine "y.employee .reports_to"}]}
           (gen "employee | employee")))
    (is (= {:table
            [{:schema "y"
              :table "employee"
              :column "reports_to"
              :parent true
              :pine "of: y.employee .reports_to"}]}
           (gen "employee | of: employee")))))

