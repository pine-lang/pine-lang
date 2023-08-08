(ns pine.hints
  (:require [clojure.string :as s]
            [pine.db.connection :as connection]
            [pine.db :as db]
            [pine.state :as state]
            [clojure.core.match :refer [match]]))

(defn abbreviate [x]
  (let [parts (s/split x #"_")
        initials (map first parts)]
    (s/lower-case (s/join "" initials)))
  )

(defn candidates [token candidates]
  (let [abbreviations (map abbreviate candidates)]
    (set
     (concat (filter #(s/includes? (s/lower-case %1) token) candidates)
             (filter #(s/includes? (abbreviate %1) token) candidates)))))

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

(defn table-hint-without-context [md op token]
  (->> [:db/references :table]
       (get-in md)
       keys
       distinct
       (candidates token)
       ))

(defn table-hint [md op token]
  (let [candidates (if (-> op :context :entity) (table-hint-with-context md op token)
                       (table-hint-without-context md op token))
        schema (op :schema)]
    (or
     (when-let [schema (op :schema)]
       ;; TODO: rule#1 for schema
       (filter (fn [x] (get-in md [:db/references :schema (name schema) :contains x])) candidates))
     candidates)
    )
  )

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
   :table table-hint})

(defn generate
  [connection op]
  (let [[token categories] (:partial op)
        md                 (connection/get-metadata connection)]
    (into {}
          (for [c categories]
            (when-let [hint-fn (hint-fns c)]
              [c (hint-fn md op token)])))))
