(ns pine.db.postgres
  (:require [pine.db.util :as u]
            ;;  temp imports
            [pine.config :as c]
            [clojure.spec.alpha :as s]
            [pine.db.protocol :refer [DbAdapter]]
            [clojure.spec.test.alpha :as stest]
            ))

;; `instrument` or `unstrument`
(stest/instrument [`references])

(defn- columns
  "Find the column names using the db connection"
  [config table]
  (->> table
       u/escape
       (format "
SELECT *
  FROM information_schema.columns
 WHERE table_name = '%s'
 -- AND table_schema = 'schema_name?'
")
       (u/exec config)
       (map :column_name)))
;; (columns c/config "user") ;; local test

(defn- refs
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
       (map (juxt :foreign_table_name
                  :column_name
                  ;; :foreign_column_name
                  ))                                            ;; (["user_id" "user"])

       ;; TODO: there can be multiple references to the foreign table
       (reduce (fn [acc [ft c]] (assoc acc (keyword ft) c)) {}) ;; { :user "user_id" }
       ))
;; (refs c/config "document")

(defn- table-definition
  "Create table definition using the db connection"
  ;; TODO: use the db name or schema name (mysql or postgres respectively)
  [config table]
  (prn (format "Loading schema definition for table: %s." table))
  {:db/columns (columns config table)
   :db/refs (refs config table)})
;; (table-definition c/config "user_tenant_role") ;; local test


(defn- get-tables [config table-catalog]
  (->> (u/exec config (format "SELECT * FROM information_schema.tables WHERE table_schema = 'public' AND table_catalog = '%s'" table-catalog))
       (map :table_name)))

;; (get-tables c/config "avallone") ;; local test

;; TODO: can the specs exist on a protocol level?
;; https://groups.google.com/g/clojure/c/f068WTgakpk

(s/def :db/columns vector?)
(s/def :db/refs map?)
(s/def :db/schema (s/keys :req [:db/columns :db/refs]))
(s/def :db/table string?)
(s/def :db/references (s/map-of keyword? string?))

(deftype PostgresAdapter [] ;; schema as an arg?
  DbAdapter
  (connection [this] (delay c/config))

  (get-schema
    [this config]
    (let [db-name     (:dbname config)
          column-name (format "tables_in_%s" db-name)
          column      (keyword column-name)]
      (prn (format "Loading schema definition for db: %s" db-name))
      (->>
       db-name
       (get-tables c/config)
       (reduce (fn [acc v] (assoc acc (keyword v) (table-definition config v))) {}))))

  (get-columns
    [this schema table-name]
    (schema (keyword table-name) :db/columns))

  (references
    [this schema table]
    (->> table
         keyword
         schema
         :db/refs))

  (quote [this x]
    (format "\"%s\"" x))

  (quote-string [this x]
    (format "'%s'" x))
  )



(s/fdef get-columns
  :args (s/cat :schema :db/schema :table :db/table)
  ;; :ret :db/references
  )

(s/fdef references
  :args (s/cat :schema :db/schema :table :db/table))
;; (references {:x {:db/columns ["user_id" "something_else"] :db/refs {"user" "user_id"}}} "x") ;; local test





