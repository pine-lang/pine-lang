(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            [pine.db :as db]
            [pine.state :as state]
            [pine.db.connection :as connection]
            [pine.hints :as hints]
            ))

;; Eval

(defn pine-prepare
  "Using a pine expression, prepare and SQL statements and params"
  [connection expression]
  (let [[sql params] (->> expression
                          (ast/str->operations connection)
                         (ast/operations->ast connection)
                         (ast/ast->sql-and-params connection))]
    {:query sql
     :params params})
  )

(defn pine-hint
  "Using a pine expression, generate hints for the last operation given"
  [connection expression]
  (->> expression
       (ast/str->operations connection)
       ;; (map (juxt :partial :context))
       last
       (hints/generate connection)
       ))

(defn pine-eval
  "Evalate an SQL query"
  [prepared]
  (let [query (prepared :query)
        params (prepared :params)
        args   (cons query params)]
    (prn args)
    (connection/query @state/c args)
    ))
