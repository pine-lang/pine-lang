(ns pine.db.postgres
  (:require [clojure.java.jdbc :as jdbc]))


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
    (rest (jdbc/query config sql opts))
    ))

(defn- index-references
  "Finding forward and inverse relations for the table Example: A 'user' has
  'document' i.e. the document has a `user_id` column that points to
  `user`.`id`. Alternatively, 'document' of 'user'.
  When we find a foreign key, then we index create both forward and inverse
  relations i.e. `:has` and `:of` relations."
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
                  (update-in [:table  table   :of  f-table :via col ] conj join)
                  ;; (update-in [:table  f-table :has table   :via col ] conj join)                            ;; Not used
                  ;;
                  ;; Case: No Ambiguity / Schema specified
                  ;;
                  ;; - Value is a single a join vector
                  ;;
                  ;; (assoc-in [:table  table    :in  schema   :refers-to f-table :in f-schema :via col] join) ;; Not used
                  ;; (assoc-in [:table  f-table  :in  f-schema :referred-by table :in schema :via col] join)   ;; Not used
                  ;;
                  ;;
                  ;; Relations between schema and tables
                  ;;
                  ;; (assoc-in [:schema schema   :contains table] true)                                        ;; Not used
                  ;; (assoc-in [:schema f-schema :contains f-table] true)                                      ;; Not used
                  )))
          {}
          references))


(defn get-indexed-references [config]
  (->> config
       get-references
       index-references))
