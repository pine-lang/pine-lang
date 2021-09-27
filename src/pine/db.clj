(ns pine.db
  (:require [clojure.java.jdbc :as jdbc]
            [pine.config :as c]
            [pine.db.mysql :as mysql]
            )
  )

;; DB wrappers

(def connection-delay (delay (mysql/pool c/config))) ;; mysql
;; (def connection-delay (delay c/config))              ;; postgres

(defn relation
  "Get the column that has the relationship between the tables:
  (relation \"caseFiles\" :owns \"documents\") => \"caseFileId\"
  (relation \"documents\" :owned-by \"caseFile\") => \"caseFileId\"
  "
  [schema t1 relationship t2]
  (case relationship
    :owns     (t1 (mysql/references schema t2))
    :owned-by (t2 (mysql/references schema t1))
    :else     nil)
  )

(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  (mysql/get-columns schema table-name))


(defn get-schema'
  [config]
  (mysql/get-schema config))

(def get-schema (memoize get-schema'))

;; DB connection

(defn connection [] @connection-delay)

;; (jdbc/query (connection) "show tables;") ;; mysql
;; (jdbc/query (connection) "\d user;")     ;; postgres

;; Helpers

(defn $
  "Execute raw sql queries:
  ($ \"select id from users limit 10\")
  ($ first \"select id from users limit 10\")
  "
  ([fn query]
   (->> query
        (jdbc/query (connection))
        fn))
  ([query]
   ($ identity query)))

(defn $!
  "Execute non select queries:
  ($ \"use tmp\")
  "
  ([fn query]
   (->> query
        (jdbc/execute! (connection))
        fn))
  ([query]
   ($! identity query)))

;; ($ "show tables")
;; ($! "use tmp")
