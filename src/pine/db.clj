(ns pine.db
  (:import (com.mchange.v2.c3p0 ComboPooledDataSource))
  (:require [clojure.java.jdbc :as j]
            [pine.config :as c]
            [clojure.string :as s])
  )

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

;; Utils

(defn escape
  "Remove non alphanumeric chars from a string"
  [s]
  (s/replace s #"[^A-Za-z0-9_-]*" ""))

;; Extract information from the schema

(defn references
  "Get the tables used in the foreign keys"
  [schema table]
  (->> table
       ((keyword table) schema)
       (re-seq #"FOREIGN KEY .`(.*)`. REFERENCES `(.*?)`")
       (map (fn [[_ col t]] { (keyword t) col }))
       (apply (partial merge-with (fn [a b] a)))
       ))

(defn relation
  "Get the column that has the relationship between the tables:
  (relation \"caseFiles\" :owns \"documents\") => \"caseFileId\"
  (relation \"documents\" :owned-by \"caseFile\") => \"caseFileId\"
  "
  [schema t1 relationship t2]
  (case relationship
    :owns     (t1 (references schema t2))
    :owned-by (t2 (references schema t1))
    :else     nil)
  )

(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  (->>
    schema
    ((keyword table-name))
    (re-seq #"(?m)^  `(\S+)`")
    (map #(second %))
  ))

;; Query the database

(defn exec
  "Execute raw sql queries"
  [connection query]
  (->> query
       (j/query connection)))

(defn table-definition
  "Create table definition"
  [config table]
  (prn (format "Loading schema definition for table: %s." table))
  (->> table
       escape
       (format "show create table %s")
       (exec config)
       first
       ((keyword "create table"))
       ))

;; (defn change-db
;;   "Change the db being used"
;;   [config db-name]
;;   (->> db-name
;;        (format "use %s")
;;        (j/execute! config)))

(defn tables
  "Get all the tables for a schema"
  [config]
  (->> "show tables"
       (exec config)
       )
  )

(defn get-schema'
  "Get the schema for the database. This function gets the schema for every table
  and can be very slow. Should be called once and the schema should be passed
  around."
  [config]
  (let [db-name     (:name config)
        column-name (format "tables_in_%s" db-name)
        column      (keyword column-name)]
    (prn (format "Loading schema definition for db: %s." db-name))
    ;; { (keyword db-name)
     (->> "show tables"
          (exec config)
          (map column)
          (map (fn [t] {(keyword t) (table-definition config t)}))
          (apply merge)
          )
     ;; }
  ))

(def get-schema (memoize get-schema'))

;; Helpers

(defn $
  "Execute raw sql queries:
  ($ \"select id from users limit 10\")
  ($ first \"select id from users limit 10\")
  "
  ([fn query]
   (->> query
        (j/query (connection))
        fn))
  ([query]
   ($ identity query)))

(defn $!
  "Execute non select queries:
  ($ \"use tmp\")
  "
  ([fn query]
   (->> query
        (j/execute! (connection))
        fn))
  ([query]
   ($! identity query)))

;; ($ "show tables")
;; ($! "use tmp")
