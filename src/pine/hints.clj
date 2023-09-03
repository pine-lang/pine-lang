(ns pine.hints
  (:require [clojure.string :as s]
            [pine.db.connection :as connection]
            [pine.db :as db]
            [pine.state :as state]
            [clojure.core.match :refer [match]]))

(defn candidates [token candidates]
  (let [xs (filter #(s/includes? (s/lower-case %1) token) candidates)]
    (->> xs
         distinct
         (sort-by count)
         )))

(defn table-hint-with-context [md op token]
  (when-let [entity (->> op :context :entity)]
    (let [entity (name entity) ;; backward compatibility - see rule#1
          ]
      (into
       ;; refers to
       (->> [:db/references :table entity :refers-to]
            (get-in md)
            keys
            (candidates token))
       ;; referred by
       (->> [:db/references :table entity :referred-by]
            (get-in md)
            keys
            (candidates token))
       ))))

(defn tables->schema-and-tables [md tables]
  (let [refs (get-in (state/md) [:db/references :table])]
    (mapcat identity
            (for [table tables
                  :let [schemas (->> table refs :in keys)]]
              (for [schema schemas]
                {:schema schema :table table})))))

(defn table-hint-without-context [md op token]
  (->> [:db/references :table]
       (get-in md)
       keys
       distinct
       (candidates token)
       ))

(defn table-hint [md op token]
  "Get all the candidates that are related to the context, or just get all the
  candidates. Conditionally, we also filter the candidates if a schema is
  specified."
  (let [xs (if (-> op :context :entity) (table-hint-with-context md op token)
               (table-hint-without-context md op token))
        schema (op :schema) ;; TODO: rule#1 for schema
        tables (if schema (filter (fn [x] (get-in md [:db/references :schema (name schema) :contains x])) xs) xs)
        ]
    tables))

(defn qualified-table-hint [md op token]
  "Get all the candidates that are related to the context, or just get all the
  candidates. Conditionally, we also filter the candidates if a schema is
  specified."
  (tables->schema-and-tables md (table-hint md op token)))

(defn schema-hint [md op token]
  (let [tables (table-hint md op "")
        schemas (reduce (fn [acc x]
                          (let [schema (->> [:db/references :table x :in]
                                            (get-in md)
                                            keys)]
                            (into acc schema)))
                        #{}
                        tables)]
    (candidates token schemas)))

(def hint-fns
  {:schema schema-hint
   :table qualified-table-hint
   })

(defn generate
  [connection op]
  (let [[token categories] (:partial op)
        md                 (connection/get-metadata connection)]
    (into {}
          (for [c categories]
            (when-let [hint-fn (hint-fns c)]
              [c (hint-fn md op token)])))))
