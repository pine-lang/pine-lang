(ns pine.db.mysql
  (:require [clojure.java.jdbc :as jdbc]
            [pine.db.util :as u]
            [pine.db.connection :refer [Connection]]

            [pine.db.connection :as connection])
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

(defn get-schema'' [config]
  (let [db-name     (:dbname config)
        column-name (format "tables_in_%s" db-name)
        column      (keyword column-name)]
    (prn (format "Loading schema definition for db: %s." db-name))
    ;; { (keyword db-name)
    (->> "show tables"
         (u/exec config) ;; use query function on the adapter
         (map column)
         (map (fn [t] {(keyword t) (table-definition config t)}))
         (apply merge))
    ;; }
    )
  )

(def get-schema-memoized (memoize get-schema''))
(defn get-schema'
  "Get schema for a given config"
  [config]
  (or (:schema config)
      (get-schema-memoized config)))

(deftype Mysql [id config]
  Connection
  (get-connection-id [this]
    id)

  (get-metadata
    [this]
    (throw (Exception. "Not implemented yet")))

  (get-tables
    [this ]
    (throw (Exception. "Not implemented yet")))

  ;; (get-columns [this table-name]
  ;;   (->>
  ;;    (get-schema' config)
  ;;    ((keyword table-name))
  ;;    (re-seq #"(?m)^  `(\S+)`")
  ;;    (map #(second %))))

  (get-references [this table]
    (->> table
         ((keyword table) (get-schema' config))
         (re-seq #"FOREIGN KEY .`(.*)`. REFERENCES `(.*?)`") ;; TODO: fix `nil` case
         (map (fn [[_ col t]] [t col nil]))                                  ;; (["user" "user_id" nil]) ;; group is nil (postgres has schemas - mysql doesn't have them)
         (group-by first)                                                    ;; {"user" ["user" "user_id"]} .. )
         (reduce (fn [acc [k v]] (assoc acc (keyword k) (map rest v))) {})   ;; {:user ["user_id"]} .. )
         ))

  (quote [this x]
    (format "`%s`" (name x)))
  (quote [this x y]
    (cond (not (nil? x)) (format "%s.%s" (connection/quote this x) (connection/quote this y))
          :else (connection/quote this y))
    )

  (quote-string [this x]
    (format "\"%s\"" x))

  (query [this statement]
    (jdbc/query config statement))

  (execute! [this statement]
    (jdbc/execute! config statement))

  (get-config [this]
    config)
  )
