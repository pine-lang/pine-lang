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
;; (stest/instrument [`references])

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
  "
  [connection]
  (prn (format "Loading all references.."))
  (let [config (connection/get-config connection)
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
-- AND ccu.foreign_table_schema=?"]
    (rest (jdbc/query config foreign-keys-sql opts)) ;; skip the first row
    ))

(defn index-references
  "Finding forward and inverse relations for the table
  Example: A 'document' is owned by a 'user' (it has a
  `user_id` column that points to `user`.`id`). When we find a
  foreign key, then we index create both forward and inverse
  relations i.e. called :points-to and :refered-by relations."
  [references]
  (reduce (fn [acc [schema table col f-schema f-table f-col]]
            (let [join [schema table col := f-schema f-table f-col]]
              (-> acc
                  ;;
                  ;; Relations between tables (No ambiguity)
                  ;; - Value is a single a join vector
                  ;;
                  (assoc-in [:table  table    :in  schema   :refers-to f-table :in f-schema :via col] join) ;; Not used
                  (assoc-in [:table  f-table  :in  f-schema :referred-by table :in schema :via col] join)   ;; Not used
                  ;;
                  ;; Relations between tables (in case of ambiguity)
                  ;; - Value is multiple join vectors
                  ;; - Even if the column to join on is not known,
                  ;;   we get a list of join vectors to choose from.
                  ;;
                  ;; TODO: this shouldn't be needed as we should be able to
                  ;; figure out which schema is being used and that value can be
                  ;; stored in the context. For now, this acts as a convenience.
                  ;;
                  (update-in [:table  table   :refers-to f-table :via col ] conj join)
                  (update-in [:table  f-table :referred-by table :via col ] conj join) ;; Not used
                  ;;
                  ;; Relations between schema and tables
                  ;;
                  (assoc-in [:schema schema   :contains table] true)
                  (assoc-in [:schema f-schema :contains f-table] true)
                  )))
          {}
          references))


(defn get-metadata [connection]
  (prn "Generating metadata")
  (let [config (connection/get-config connection)
        fixtures (config :fixtures)
        references (if fixtures (fixtures :relations) (get-references connection))
        ]
    {:db/references (index-references references)}))

(def get-metadata-memoized (memoize get-metadata))

;; TODO: can the specs exist on a protocol level?
;; https://groups.google.com/g/clojure/c/f068WTgakpk

(s/def :db/schema (s/keys :req [:db/columns :db/foreign-keys]))
(s/def :db/table string?)
(s/def :db/references (s/map-of keyword? string?))

(defn string->uuid [x]
  (try
    (java.util.UUID/fromString x)
    (catch Exception e x)))


(defn string->number [x]
  (try
    (Integer/parseInt x)
    (catch Exception e x)))

(defn try-conversion [x]
  (or (string->number x)
      (string->uuid x)
      x))

(deftype Postgres [id config]
  Connection

  (get-connection-id [this]
    id)

  (get-metadata [this]
    (get-metadata-memoized this))

  (get-columns
    [this table-name]
    (throw (Exception. "Not implemented yet")))

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
                      (map try-conversion) ;; hack: attempt to convert to the appropriate type
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





