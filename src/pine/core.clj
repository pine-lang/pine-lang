(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            [pine.config :as c]
            )
  )

;; Eval

(defn pine-prepare
  "Prepare a pine expression i.e. get the sql query and params"
  [expression]
  (let [[sql params] (-> expression
                         ast/str->operations
                         ast/operations->ast
                         ast/ast->sql-and-params)]
    {:query sql
     :params params})
  )


(defn pine-eval
  "Evalate a query"
  [db expression]
  (let [prepared (pine-prepare expression)
        query (prepared :query)
        params (prepared :params)
        args   (cons query params)
        ]
    (j/query db args)
    ))

;; Helpers

(defn $
  "Evaluate Pine expressions:
  ($ count \"users *\")
  ($ first \"users *\")
  ($ (partial map :fullName) \"users *\")
  "
  ([fn expression]
   (->> expression
        (pine-eval c/db)
        fn))
  ([expression]
   ($ (fn[x] x) expression)))
