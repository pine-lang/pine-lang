(ns pine.db.mysql
  (:require [pine.db.util :as u]
            [pine.config :as c]
            [pine.db.protocol :refer [DbAdapter]]
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

(deftype MysqlAdapter [] ;; schema as an arg?
  DbAdapter
  (connection [this] (delay (pool c/config)))

  (get-schema [this config]
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

  (get-columns [this schema table-name]
    (->>
     schema
     ((keyword table-name))
     (re-seq #"(?m)^  `(\S+)`")
     (map #(second %))
     )
    )


  (references [this schema table]
    (->> table
         ((keyword table) schema)
         (re-seq #"FOREIGN KEY .`(.*)`. REFERENCES `(.*?)`") ;; TODO: fix `nil` case
         (map (fn [[_ col t]] { (keyword t) col }))
         (apply (partial merge-with (fn [a b] a))) ;; TODO: fix `nil` case
         )
    )

  (quote [this x]
    (format "`%s`" x))

  (quote-string [this x]
    (format "\"%s\"" x))
  )
