(ns pine.db.postgres
  (:require [clojure.java.jdbc :as jdbc]
            [pine.db.util :as u]
            ;;  temp imports
            [pine.config :as config]
            [clojure.spec.alpha :as s]
            [pine.db.connection :refer [Connection]]
            [clojure.spec.test.alpha :as stest]
            [pine.db.connection :as connection]))

;; `instrument` or `unstrument`
(stest/instrument [`references])

;; OBSOLETE: I was originally using this funciton to fix the order of the
;; columns which was an unrelated issue. Getting the columns explicitly is not
;; needed until I add support for the columns in the hints. Untill then I will
;; leave this out.
;;
;; (defn- columns
;;   "Find the column names using the db connection.
;;   "
;;   ([config table table-group]
;;    (->> (format
;;          "SELECT * FROM information_schema.columns WHERE table_name = '%s' AND table_schema = '%s' "
;;          (u/escape table) (u/escape table-group))
;;         (u/exec config)
;;         (map :column_name)))
;;   ([config table]
;;    (columns config table "public")))

(defn get-references
  "Get the foreign keys from the database.
  Finding forward and inverse relations for the table
  Example: A 'document' is owned by a 'user' (it has a
                                                 `user_id` column that points to `user`.`id`). When we find a
  foreign key, then we index create both forward and inverse
  relations i.e. called :points-to and :refered-by relations.
  "
  [connection]
  (let [_ (prn "Loading all relations")
        config (connection/get-config connection)
        opts {:as-arrays? true}
        foreign-keys-sql "SELECT
  kcu.table_schema,
  kcu.table_name,
  kcu.column_name,
  ccu.table_schema AS foreign_table_schema,
  ccu.table_name AS foreign_table_name,
  ccu.column_name AS foreign_column_name
 FROM information_schema.table_constraints AS tc
 JOIN information_schema.key_column_usage AS kcu
   ON tc.constraint_name = kcu.constraint_name
 LEFT
 JOIN information_schema.constraint_column_usage AS ccu
   ON ccu.constraint_name = tc.constraint_name
WHERE tc.constraint_type = 'FOREIGN KEY'
-- AND tc.table_name=?
-- AND ccu.foreign_table_schema=?"
        rows (jdbc/query config foreign-keys-sql opts)]
    (reduce (fn [acc [schema table col f-schema f-table f-col]]
              (-> acc
                  (assoc-in [schema table :with col :points-to f-table] [f-schema f-table f-col])
                  (assoc-in [schema f-table :referred-by table :with col] [f-schema f-table f-col])))
            {}
            rows)))

(defn get-foreign-keys-deprecated
  "Find the foreign keys using the db connection"
  [config table table-group]
   (->> (format "
SELECT
kcu.table_schema, -- TODO: left it here ... along with the schema, I need to get the schema and table as well
kcu.table_name,   --       also this...
kcu.column_name,
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

(defn table-definition
  "Create table definition using the db connection
  TODO: also return the table group (postgres schema)"
  [config table table-group]
  (prn (format "Loading schema: %s.%s" table-group table))
  {:db/foreign-keys (get-foreign-keys-deprecated config table table-group)})


(defn- get-tables' [config table-catalog]
  (->> (u/exec config (format "SELECT table_schema, table_name FROM information_schema.tables WHERE table_catalog = '%s'
 AND table_schema IN ('public', 'types', 'security', 'requests', 'screening', 'questions', 'signatures', 'data_request') " table-catalog))
       (map (juxt :table_schema :table_name))))
(def get-tables-memoized (memoize get-tables'))



;; TODO: can the specs exist on a protocol level?
;; https://groups.google.com/g/clojure/c/f068WTgakpk

(s/def :db/foreign-keys map?)
(s/def :db/schema (s/keys :req [:db/columns :db/foreign-keys]))
(s/def :db/table string?)
(s/def :db/references (s/map-of keyword? string?))

(defn get-schema'' [config]
  (let [db-name     (:dbname config)
        column-name (format "tables_in_%s" db-name)
        column      (keyword column-name)
        tables      (get-tables-memoized config db-name)]
    (prn (format "Loading schema definition for db: %s" db-name))
    (reduce (fn [acc [table-group table]]
              (assoc acc (keyword table) (table-definition config table table-group)))
            {} tables)))

(def get-schema-memoized (memoize get-schema''))

(defn get-schema'
  "Get schema for a given config"
  [config]
  (or (config :schema)
      (get-schema-memoized config)))

(defn string->uuid [x]
  (try
    (java.util.UUID/fromString x)
    (catch Exception e x)))

(deftype Postgres [id config]
  Connection

  (get-connection-id [this]
    id)

  (get-tables
    [this]
    (if (config :schema)
      (throw (Exception. "You seem to be using a hard coded schema (used for testing). `get-tables` is not supported in this mode yet."))
      (get-tables-memoized config (:dbname config)))
    )

  (get-columns
    [this table-name]
    (throw (Exception. "Not implemented yet")))

  (references
    [this table]
    (let [schema (get-schema' config)]
      (->> table
           keyword
           schema
           :db/foreign-keys)))

  (quote [this x]
    (format "\"%s\"" (name x)))
  (quote [this x y]
    (cond (not (nil? x)) (format "%s.%s" (connection/quote this x) (connection/quote this y))
          :else (connection/quote this y))
    )


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
      result))

  (execute! [this statement]
    (jdbc/execute! config statement))

  (get-config [this]
    config)

  )

(s/fdef get-columns
  :args (s/cat :schema :db/schema :table :db/table)
  ;; :ret :db/references
  )

(s/fdef references
  :args (s/cat :schema :db/schema :table :db/table))
;; (references {:x {:db/columns ["user_id" "something_else"] :db/foreign-keys {"user" "user_id"}}} "x") ;; local test





