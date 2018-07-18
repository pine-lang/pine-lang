(ns pine.db
  (:require [clojure.java.jdbc :as j]
            [pine.config :as c]
            [clojure.string :as s])
  )

(defn exec
  "Execute raw sql queries"
  [db query]
  (->> query
       (j/query db)))

(defn escape
  "Remove non alphanumeric chars from a string"
  [s]
  (s/replace s #"[^A-Za-z0-9_-]*" ""))

(defn table-definition
  "Create table definition"
  [db table]
  (->> table
      escape
      (format "show create table %s")
      (exec db)
      first
      ((keyword "create table"))
      ))

(defn references
  "Get the tables used in the foreign keys"
  [db table]
  (->> table
       (table-definition db)
       (re-seq #"FOREIGN KEY .`(.*)`. REFERENCES `(.*?)`")
       (map (fn [[_ col t]] { (keyword t) col }))
       (apply merge)
       ))

(defn relation
  "Get the column that has the relationship between the tables:
  (relation \"caseFiles\" :owns \"documents\") => \"caseFileId\"
  (relation \"documents\" :owned-by \"caseFile\") => \"caseFileId\"
  "
  [db t1 relationship t2]
  (case relationship
    :owns     ((keyword t1) (references db t2))
    :owned-by ((keyword t2) (references db t1))
    :else     nil)
  )

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
