(ns pine.db.postgres
  (:require [clojure.java.jdbc :as jdbc]
            [pine.db.util :as u]
            ;;  temp imports
            [pine.config :as c]
            [clojure.spec.alpha :as s]
            [pine.db.protocol :refer [Connection]]
            [clojure.spec.test.alpha :as stest]))

;; `instrument` or `unstrument`


(stest/instrument [`references])

(defn- columns
  "Find the column names using the db connection"
  ([config table table-group]
   (->> (format
         "SELECT * FROM information_schema.columns WHERE table_name = '%s' AND table_schema = '%s' "
         (u/escape table) (u/escape table-group))
        (u/exec config)
        (map :column_name)))
  ([config table]
   (columns config table "public")))

;; (columns (->> c/config :connections :?) "request" "requests") ;; local test
;; (columns (->> c/config :connections :?) "user") ;; local test

(defn refs
  "Find the foreign keys using the db connection"
  ([config table table-group]
   (->> (format "
SELECT kcu.column_name,
ccu.table_name AS foreign_table_name,
ccu.column_name AS foreign_column_name,
ccu.table_schema AS foreign_table_schema
FROM information_schema.table_constraints AS tc
JOIN information_schema.key_column_usage AS kcu
ON tc.constraint_name = kcu.constraint_name
LEFT JOIN information_schema.constraint_column_usage AS ccu
ON ccu.constraint_name = tc.constraint_name
WHERE tc.table_name='%s'
-- AND ccu.foreign_table_schema='%s'
AND tc.constraint_type = 'FOREIGN KEY'
" (u/escape table) (u/escape table-group))
        (u/exec config)
        (map (juxt :foreign_table_name :column_name :foreign_table_schema)) ;; (["user"  "user_id" "public"])
        (group-by first)                                                    ;; { "user" ["user" "user_id" "public"]}.. )
        (reduce (fn [acc [k v]] (assoc acc (keyword k) (map rest v))) {})   ;; { :user  [["user_id" "public"]]}..)
        ))
  ([config table]
   (refs config table "public")))

;; (refs (->> c/config :connections :work) "document")

(defn table-definition
  "Create table definition using the db connection
  TODO: also return the table group (postgres schema)"
  [config table table-group]
  (prn (format "Loading schema: %s.%s" table-group table))
  {:db/columns (columns config table table-group)
   :db/refs (refs config table table-group)})
;; (table-definition c/config "user_tenant_role") ;; local test


(defn- get-tables [config table-catalog]
  (->> (u/exec config (format "SELECT table_schema, table_name FROM information_schema.tables WHERE table_catalog = '%s'
 AND table_schema IN ('public', 'types', 'security', 'requests', 'screening', 'questions', 'signatures', 'data_request') " table-catalog))
       (map (juxt :table_schema :table_name))))
(def get-tables-memoized (memoize get-tables))

;; TODO: can the specs exist on a protocol level?
;; https://groups.google.com/g/clojure/c/f068WTgakpk

(s/def :db/columns vector?)
(s/def :db/refs map?)
(s/def :db/schema (s/keys :req [:db/columns :db/refs]))
(s/def :db/table string?)
(s/def :db/references (s/map-of keyword? string?))

(defn get-schema' [config]
  (let [db-name     (:dbname config)
        column-name (format "tables_in_%s" db-name)
        column      (keyword column-name)
        tables      (get-tables-memoized config db-name)
        ]
    (prn (format "Loading schema definition for db: %s" db-name))
    (reduce (fn [acc [table-group table]]
              (assoc acc (keyword table) (table-definition config table table-group))
              )
            {} tables)
    ))
(def get-schema-memoized (memoize get-schema'))

(defn string->uuid [x]
  (try
    (java.util.UUID/fromString x)
    (catch Exception e x)))

(deftype Postgres [id config]
  Connection

  (get-connection-id [this]
    id)

  (get-schema
    [this]
    (get-schema-memoized config))

  (get-tables
    [this]
    (get-tables-memoized config (:dbname config)))

  (get-columns
    [this schema table-name]
    (->> table-name
         keyword
         schema
         :db/columns
         ))

  (references
    [this schema table]
    (->> table
         keyword
         schema
         :db/refs
         ))

  (quote [this x]
    (format "\"%s\"" x))

  (quote-string [this x]
    (format "'%s'" x))

  (query [this statement]
    ;; args is [ [:string ".."] ] which works for mysql
    ;; However, it doesn't work for postgres, we need to convert that to [ ".." ]
    (let [query (first statement)
          params (->> statement
                      rest
                      (map second)
                      (map string->uuid) ;; hack: attempt to convert to uuid
                      )
          s (cons query params)
          result (jdbc/query config s {:as-arrays? true})]
      result
      ))

  (execute! [this statement]
    (jdbc/execute! config statement)))

(s/fdef get-columns
  :args (s/cat :schema :db/schema :table :db/table)
  ;; :ret :db/references
  )

(s/fdef references
  :args (s/cat :schema :db/schema :table :db/table))
;; (references {:x {:db/columns ["user_id" "something_else"] :db/refs {"user" "user_id"}}} "x") ;; local test





