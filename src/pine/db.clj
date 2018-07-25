(ns pine.db
  (:require [clojure.java.jdbc :as j]
            [pine.config :as c]
            [clojure.string :as s])
  )

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
       (apply merge)
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


;; Query the database

(defn exec
  "Execute raw sql queries"
  [db-config query]
  (->> query
       (j/query db-config)))

(defn table-definition
  "Create table definition"
  [db-config table]
  (->> table
       escape
       (format "show create table %s")
       (exec db-config)
       first
       ((keyword "create table"))
       ))

(defn schema-
  "Get the schema for the database. This function gets the schema for every table
  and can be very slow. Should be called once and the schema should be passed
  around."
  [db-config db-name ]
  (let [column-name (format "tables_in_%s" db-name)
        column      (keyword column-name)]
    (->> "show tables"
         (exec db-config)
         (map column)
         (map (fn [c] {(keyword c) (table-definition db-config c)}))
         (apply merge)
    )
  ))

(def schema (memoize schema-))

;; Helpers

(defn $
  "Execute raw sql queries:
  ($ \"select id from users limit 10\")
  ($ first \"select id from users limit 10\")
  "
  ([fn query]
   (->> query
        (j/query c/db)
        fn))
  ([query]
   ($ (fn[x] x) query)))
