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

(deftest test-hints
  (testing "Generate hints"
    (is (= [{:schema "x", :table "company"
             :pine "x.company"}]
           (-> "co" gen :table)))

    (is (= [{:schema "y", :table "employee" :column "company_id" :parent false
             :pine "y.employee .company_id"}
            {:schema "z", :table "document", :column "company_id", :parent false,
             :pine "z.document .company_id"}]
           (-> "company | e" gen :table)))

    (is (= [{:schema "x", :table "company" :column "company_id" :parent true
             :pine "of: x.company .company_id"}]
           ;; (gen "employee | co")
           (-> "employee | co" gen :table)))

    (is (= []
           (-> "company as c | s: id" gen :table)))

;; The following shouldn't generate any hint but it does
    ;;
    ;; (is (= {:table []}
    ;;        (gen "company as c")))
    )

  (testing "Generate hints in ambiguity"
    (is (= [{:schema "z",
             :table "document"
             :column "employee_id"
             :parent false
             :pine "z.document .employee_id"}
            {:schema "z"
             :table "document"
             :column "created_by"
             :parent false
             :pine "z.document .created_by"}]
           (-> "employee | doc" gen :table))))

  (testing "Generate hints when direction is specified"
    (is (= [{:schema "y"
             :table "employee"
             :column "reports_to"
             :parent true
             :pine "of: y.employee .reports_to"}
            {:schema "y"
             :table "employee"
             :column "reports_to"
             :parent false
             :pine "y.employee .reports_to"}]
           (-> "employee | employee" gen :table)))
    (is (= [{:schema "y"
             :table "employee"
             :column "reports_to"
             :parent true
             :pine "of: y.employee .reports_to"}]
           (-> "employee | of: employee" gen :table))))

  (testing "Generate `select` hints with relevant columns"
    (is (= []
           (-> "x.company | s: does_not_exist" gen :select)))
    (is (= [{:column "id" :alias "c_0"}]
           (-> "x.company | s: i" gen :select)))
    (is (= ["company_id" "id"] ;;  "reports_to" is not returned
           (->> "y.employee | s: id" gen :select (map :column)))))

  (testing "Generate `select-partial` hints"
    (is (= [{:column "id" :alias "c_0"}]     (->  "company    | s:"                 gen :select)))
    (is (= [{:column "id" :alias "c_0"}]     (->  "x.company  | s:"                 gen :select)))
    (is (= ["reports_to"  "company_id" "id"] (->> "y.employee | s:"                 gen :select (map :column))))
    (is (= ["reports_to" ]                   (->> "y.employee | s: id, company_id," gen :select (map :column))))))

