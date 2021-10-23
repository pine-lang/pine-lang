(ns pine.db.postgres
  (:require [pine.db.util :as u]
            ;;  temp imports
            [pine.config :as c]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

;; `instrument` or `unstrument`
;; (stest/instrument [`references])


(defn- not-implemented [name]
  (throw (Exception. (str "Not implemented :: " name))))

(s/def :db/schema map?)
(s/def :db/table string?)
(s/def :db/references (s/map-of keyword? string?))

(defn references
  "Get the tables used in the foreign keys"
  [schema table]
  ;; {:pre [(s/valid? map? schema) (s/valid? :db/table table)]
  ;;  :post [(s/valid? :db/references %)]}

  ;; TODO: implement the following
  ;;       it should be all the keys in the schema map with a value
  {:test (keyword table)}) ;; TODO: update this!
(s/fdef references
  :args (s/cat :schema :db/schema :table :db/table)
  ;; :ret :db/references
  )
;; (references "a" "b") ;; local test


(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  ;; TODO: this should be all the keys in the schema map
  (not-implemented "get-columns"))

(defn- columns
  "Find the column names using the db connection"
  [config table]
  (->> "user_tenant_role"
       u/escape
       (format "
SELECT *
  FROM information_schema.columns
 WHERE table_name = '%s'
 -- AND table_schema = 'schema_name?'
")
       (u/exec config)
       (map :column_name)
       ))

(defn- foreign-keys
  "Find the foreign keys using the db connection"
  [config table]
  (->> table
       u/escape
       (format "
 SELECT kcu.column_name,
        ccu.table_name AS foreign_table_name,
        ccu.column_name AS foreign_column_name
   FROM information_schema.table_constraints AS tc
   JOIN information_schema.key_column_usage AS kcu
     ON tc.constraint_name = kcu.constraint_name
   LEFT JOIN information_schema.constraint_column_usage AS ccu
     ON ccu.constraint_name = tc.constraint_name
  WHERE tc.table_name='%s'
    AND tc.constraint_type = 'FOREIGN KEY'
")
       (u/exec config)
       (map (juxt :column_name :foreign_table_name :foreign_column_name))))


(defn- table-definition
  "Create table definition using the db connection"
  ;; TODO: use the db name or schema name (mysql or postgres respectively)
  [config table]
  (prn (format "Loading schema definition for table: %s." table))
  (let [cs (columns config table)
        fks (foreign-keys config table)]
    [cs fks]) ;; TODO: merge these into one!
  )

;; (table-definition c/config "user_tenant_role")

;; (->>
;;  (table-definition c/config "user_tenant_role")
;;  (map (fn [[x y z]] [x (format "\"%s\".\"%s\"" y z)])))

(defn get-tables [config table-catalog]
  (->> (u/exec config (format "SELECT * FROM information_schema.tables WHERE table_schema = 'public' AND table_catalog = '%s'" table-catalog))
       (map :table_name)))

;; (get-tables c/config "avallone") ;; local test


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
    (->>
     db-name
     (get-tables c/config)
          ;; TODO: this is in progress... maybe use a reduce here?
     (reduce (fn [acc v] (assoc acc (keyword v) "this is a test")) {})
     ;; (map (fn [t] {(keyword t) (table-definition config t)})) ;; TODO: implement
     ;; (apply merge)
     )
     ;; }
    ))
