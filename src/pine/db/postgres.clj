(ns pine.db.postgres
  (:require [clojure.java.jdbc :as jdbc]
            [pine.db.config :as config]
            [pine.db.fixtures :as fixtures]
            [pine.util :as util]))

(defn- get-references
  "Get the foreign keys from the database."
  [config]
  (prn (format "Loading all references..."))
  (let [opts {:as-arrays? true}
        sql "SELECT
  n.nspname AS table_schema,
  c.relname AS table_name,
  a.attname AS column_name,
  fn.nspname AS foreign_table_schema,
  f.relname AS foreign_table_name,
  fa.attname AS foreign_column_name
FROM pg_constraint con
JOIN pg_class c ON c.oid = con.conrelid
JOIN pg_namespace n ON n.oid = c.relnamespace
JOIN pg_attribute a ON a.attnum = ANY(con.conkey) AND a.attrelid = c.oid
JOIN pg_class f ON f.oid = con.confrelid
JOIN pg_namespace fn ON fn.oid = f.relnamespace
JOIN pg_attribute fa ON fa.attnum = ANY(con.confkey) AND fa.attrelid = f.oid
WHERE con.contype = 'f'
"]
    (rest (jdbc/query config sql opts))))

(defn- index-references
  "Finding forward and inverse relations for the table Example: A 'user' has
  'document' i.e. the document has a `user_id` column that points to
  `user`.`id`. Alternatively, 'document' of 'user'. When we find a foreign key,
  then we index create both forward and inverse relations i.e. `:has` and `:of`
  relations / or `:refered-by` and `:refers-to` relations."
  [references]
  (reduce (fn [acc [schema table col f-schema f-table f-col]]
            (let [join [schema table col := f-schema f-table f-col]]
              (-> acc
                  ;; Case: Ambiguity / Schema not specified
                  ;;
                  ;; Relations between tables (in case of ambiguity)
                  ;; - Value is multiple join vectors
                  ;; - Even if the column to join on is not known,
                  ;;   we get a list of join vectors to choose from.
                  ;;
                  ;; This shouldn't be needed as we should be able to
                  ;; figure out which schema is being used and that value can be
                  ;; stored in the context. For now, this is convenient. For
                  ;; consider the 'No ambiguity' approach below
                  ;;
                  (update-in [:table  table   :refers-to f-table :via col] conj join)
                  (update-in [:table  f-table :referred-by table :via col] conj join)
                  ;;
                  ;; Case: No Ambiguity / Schema specified
                  ;;
                  ;; - Value is a single a join vector
                  ;;
                  (assoc-in [:table  table    :in  schema   :refers-to f-table :in f-schema :via col] join)
                  (assoc-in [:table  f-table  :in  f-schema :referred-by table :in schema :via col] join)
                  ;;
                  ;;
                  ;; Relations between schema and tables
                  ;; TODO: check if this is used
                  (assoc-in [:schema schema   :contains table] true)
                  (assoc-in [:schema f-schema :contains f-table] true))))
          {}
          references))

(defn get-references-helper [id]
  (let [connection (util/get-connection id)]
    (get-references connection)))

(defn get-indexed-references [id]
  (let [references (cond
                     (= id :test) fixtures/references
                     :else (get-references-helper id))]
    (index-references references)))

(defn run-query [id query]
  (let [connection (config/connections id)
        {:keys [query params]} query]
    (jdbc/query connection (cons query params) {:as-arrays? true :identifiers identity})))
