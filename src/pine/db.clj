(ns pine.db
  (:require [clojure.java.jdbc :as jdbc]
            [pine.config :as c]
            [pine.db.mysql :as mysql]
            )
  (:import com.mchange.v2.c3p0.ComboPooledDataSource))

;; Connection Pooling
(defn pool
  [spec]
  (let [cpds (doto (ComboPooledDataSource.)
               (.setDriverClass (:classname spec))
               (.setJdbcUrl (str "jdbc:" (:subprotocol spec) ":" (:subname spec)))
               (.setUser (:user spec))
               (.setPassword (:password spec))
               ;; expire excess connections after 30 minutes of inactivity:
               (.setMaxIdleTimeExcessConnections (* 30 60))
               ;; expire connections after 3 hours of inactivity:
               (.setMaxIdleTime (* 3 60 60))
               )
        ]
    {:datasource cpds}))
(def pooled-db (delay (pool c/config)))
(defn connection [] @pooled-db)

;; Multiplexers

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
