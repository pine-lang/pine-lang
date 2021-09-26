(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            [pine.db :as db]))

;; Eval

(defn pine-prepare
  "Prepare a pine expression i.e. get the sql query and params"
  [schema expression]
  (let [[sql params] (->> expression
                         ast/str->operations
                         (ast/operations->ast schema)
                         ast/ast->sql-and-params)]
    {:query sql
     :params params})
  )

;; (pine-prepare (db/get-schema c/config) "caseFiles 1 | delete!")
;; (pine-prepare (db/get-schema c/config) "caseFiles 1 | s: id")


(defn pine-eval
  "Evalate a query"
  [connection prepared]
  (let [query (prepared :query)
        params (prepared :params)
        args   (cons query params)]
    (j/query connection args)
    ))

;; Helpers

(defn $
  "Evaluate Pine expressions:
  ($ count schema \"users *\")
  ($ first schema \"users *\")
  ($ (partial map :fullName) schema \"users *\")
  "
  ([fn schema expression]
   (->> expression
        (pine-prepare schema)       ;; prepare the sql for executing using cached schema
        (pine-eval (db/connection)) ;; execute
        fn))
  ([schema expression]
   ($ (fn[x] x) schema expression)))

