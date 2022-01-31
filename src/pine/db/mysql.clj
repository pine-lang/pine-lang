(ns pine.db.mysql
  (:require [pine.db.util :as u]
            [pine.config :as c]
            )
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  )

;; Connection Pooling
(defn- pool
  [spec]
  (let [cpds (doto (ComboPooledDataSource.)
               (.setDriverClass (:classname spec))
               (.setJdbcUrl (str "jdbc:" (:dbtype spec) ":" (:subname spec)))
               (.setUser (:user spec))
               (.setPassword (:password spec))
               ;; expire excess connections after 30 minutes of inactivity:
               (.setMaxIdleTimeExcessConnections (* 30 60))
               ;; expire connections after 3 hours of inactivity:
               (.setMaxIdleTime (* 3 60 60))
               )
        ]
    {:datasource cpds}))


(defn references
  "Get the tables used in the foreign keys"
  [schema table]
  (->> table
       ((keyword table) schema)
       (re-seq #"FOREIGN KEY .`(.*)`. REFERENCES `(.*?)`") ;; TODO: fix `nil` case
       (map (fn [[_ col t]] { (keyword t) col }))
       (apply (partial merge-with (fn [a b] a))) ;; TODO: fix `nil` case
       ))

(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  (->>
    schema
    ((keyword table-name))
    (re-seq #"(?m)^  `(\S+)`")
    (map #(second %))
  ))

(defn- table-definition
  "Create table definition"
  [config table]
  (prn (format "Loading schema definition for table: %s." table))
  (->> table
       u/escape
       (format "show create table %s")
       (u/exec config)
       first
       ((keyword "create table"))
       ))

(defn get-schema
  "Get the schema for the database. This function gets the schema for every table
  and can be very slow. Should be called once and the schema should be passed
  around."
  [config]
  (let [db-name     (:dbname config)
        column-name (format "tables_in_%s" db-name)
        column      (keyword column-name)]
    (prn (format "Loading schema definition for db: %s." db-name))
    ;; { (keyword db-name)
     (->> "show tables"
          (u/exec config)
          (map column)
          (map (fn [t] {(keyword t) (table-definition config t)}))
          (apply merge)
          )
     ;; }
  ))


(defn quote [x]
  "Handle table names, columns names, etc."
  (format "`%s`" x))

(defn quote-string [x]
  (format "\"%s\"" x))

(defn connection[] (delay (pool c/config)))
