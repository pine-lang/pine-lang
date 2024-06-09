(ns pine.eval.sql
  (:require
   [pine.parser :as parser]
   [pine.ast.main :as ast]))

(defn generate [ast]
  (let [{:keys [tables joins limit aliases]} ast
        base-table (first tables)
        base-alias (:alias base-table)
        from-clause (str "FROM " (:table base-table) " AS " base-alias)
        join-clauses (for [[a1 join-map] joins
                           [a2 condition] join-map]
                       (let [[_ col1 _ _ col2] condition
                             ;; Lookup table name using aliases map
                             t2 (:table (aliases a2))]
                         (str "JOIN " t2 " AS " a2
                              " ON " a1 "." col1 " = " a2 "." col2)))
        join-clause (clojure.string/join " " join-clauses)
        limit-clause (if limit (str "LIMIT " limit) "")
        select-clause "SELECT *"]
    (str select-clause " " from-clause " " join-clause " " limit-clause ";")))

;; (->> "company as c |  tenant as t | user_tenant_role as r "
;;      parser/parse
;;      ast/generate
;;      ;; :joins
;;      generate
;;      )


