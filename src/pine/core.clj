(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            [pine.config :as c]
            [pine.fixtures :as fixtures]
            [pine.db :as db])
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


;; TODO: make sure that the db connection is reused instead of creating a new one with each evaluation.
;;
(defn pine-eval
  "Evalate a query"
  [db prepared]
  (let [query (prepared :query)
        params (prepared :params)
        args   (cons query params)]
    (j/query db args)
    ))

;; Helpers

;; Load the schema only once
(def *schema* (db/schema "INSERT_DB_NAME" c/db))

(defn $
  "Evaluate Pine expressions:
  ($ count \"users *\")
  ($ first \"users *\")
  ($ (partial map :fullName) \"users *\")
  "
  ([fn expression]
   (->> expression
        (pine-prepare *schema*) ;; prepare the sql for executing using cached schema
        (pine-eval c/db)        ;; execute
        fn))
  ([expression]
   ($ (fn[x] x) expression)))

(defn $prepare
  "Show the query to be executed"
  [expression]
  (->> expression
       (pine-prepare *schema*) ;; prepare the sql for executing using cached schema
       ))

;; ($prepare "documents title=1* | caseFiles *")
