(ns pine.core
  (:require [clojure.java.jdbc :as j]
            [pine.ast :as ast]
            )
  )

;; Config

(let

    [db-host "127.0.0.1"
     db-port 3306
     db-name "?"
     db-user "?"
     db-password "?"]

  (def db {:classname "com.mysql.jdbc.Driver"
           :subprotocol "mysql"
           :subname (str "//" db-host ":" db-port "/" db-name)
           :user db-user
           :password db-password}))




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

;; (->> "caseFiles 54 | documents sample"
;;    (pine-eval db)
;;    first
;;    :title_2
;;    )

