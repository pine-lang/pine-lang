(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            [pine.fixtures :as fixtures]
            [pine.db :as db]
            [clojure.string :as s])
  )

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

;; ($prepare (db/schema c/db) "caseFiles 1 | delete!")

;; ($prepare (db/schema c/db) "caseFiles 1 | s: id")

(defn pine-eval
  "Evalate a query"
  [connection prepared]
  (let [query (prepared :query)
        params (prepared :params)
        args   (cons query params)]
    (j/query connection args)
    ))

;; Helpers

;; Load the schema only once
;; (def *schema* (db/schema c/db "INSERT_DB_NAME" ))

(defn $
  "Evaluate Pine expressions:
  ($ count \"users *\")
  ($ first \"users *\")
  ($ (partial map :fullName) \"users *\")
  "
  ([fn schema expression]
   (->> expression
        (pine-prepare schema)       ;; prepare the sql for executing using cached schema
        (pine-eval (db/connection)) ;; execute
        fn))
  ([schema expression]
   ($ (fn[x] x) schema expression)))

(defn $prepare
  "Prepare query and the parameters"
  [schema expression]
  (->> expression
       (pine-prepare schema) ;; prepare the sql for executing using cached schema
       ))

;; ($prepare (db/schema c/db) "penneo_com.caseFiles 1")
