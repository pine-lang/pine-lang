(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            [pine.db :as db]
            [pine.db.connection :as connection]
            [pine.hints :as hints]
            ))

;; Eval

;; TODO: this shouldn't take the schema. Schema is embedded in the adapter (i.e
;; connection)
(defn pine-prepare
  "Using a pine expression, prepare and SQL statements and params"
  [schema expression]
  (let [[sql params] (->> expression
                         ast/str->operations
                         (ast/operations->ast schema)
                         ast/ast->sql-and-params)]
    {:query sql
     :params params})
  )

(defn pine-hint
  "Using a pine expression, generate hints for the last operation given"
  [expression]
  (->> expression
       ast/str->operations
       ;; (map (juxt :partial :context))
       last
       hints/generate
       ))

;; (->> "tenant | company"
;;      ast/str->operations
;;      (map (juxt :partial :context))
;;      last
;;      (apply db/hints)
;;      )

;; (pine-prepare (db/get-schema c/config) "caseFiles 1 | delete!")
;; (pine-prepare (db/get-schema c/config) "caseFiles 1 | s: id")


(defn pine-eval
  "Evalate an SQL query"
  [prepared]
  (let [query (prepared :query)
        params (prepared :params)
        args   (cons query params)]
    (prn args)
    (connection/query @db/connection args)
    ))
