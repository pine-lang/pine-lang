(ns pine.db
  (:require [clojure.java.jdbc :as jdbc]
            [pine.config :as c]
            [pine.db.mysql :as mysql ]
            [pine.db.postgres :as postgres]
            [pine.db.protocol :as protocol]
            )
  (:import pine.db.mysql.MysqlAdapter)
  (:import pine.db.postgres.PostgresAdapter)
  )

;; (ns-unalias 'pine.db 'dbadapter)

(def dbadapter (let [type (c/config :dbtype)]
                 (cond (= type "mysql" ) (MysqlAdapter.)
                       (= type "postgres") (PostgresAdapter.)
                       :else (throw (Exception. (format "Db not supported: %s" type))))))

;; DB wrappers
(defn quote [x]
  (protocol/quote dbadapter x))

(defn quote-string [x]
  (protocol/quote-string dbadapter x))

(defn references [schema table]
  (protocol/references dbadapter schema table))

(defn relation
  "Get the column that has the relationship between the tables:
  (relation :caseFiles: :owns :documents:) => \"caseFileId\"
  (relation :documents: :owned-by :caseFile:) => \"caseFileId\"
  "
  [schema t1 relationship t2]
  (prn t1)
  (case relationship
    :owns     (t1 (protocol/references dbadapter schema t2))
    :owned-by (t2 (protocol/references dbadapter schema t1))
    :else     nil)
  )

(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  (protocol/get-columns dbadapter schema table-name))


(defn get-schema'
  [config]
  (protocol/get-schema dbadapter config))

(def get-schema (memoize get-schema'))

;; DB connection

(defn connection [] @(protocol/connection dbadapter))

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
