(ns pine.db.mysql
  (:require [pine.db.util :as u]))

(defn table-definition
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

(defn references
  "Get the tables used in the foreign keys"
  [schema table]
  (->> table
       ((keyword table) schema)
       (re-seq #"FOREIGN KEY .`(.*)`. REFERENCES `(.*?)`")
       (map (fn [[_ col t]] { (keyword t) col }))
       (apply (partial merge-with (fn [a b] a)))
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

(defn get-schema
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
          (u/exec config)
          (map column)
          (map (fn [t] {(keyword t) (table-definition config t)}))
          (apply merge)
          )
     ;; }
  ))
