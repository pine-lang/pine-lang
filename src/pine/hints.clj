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

(defn schema-hint [connection op token]
  (let [md (connection/get-metadata connection)]
    (->> [:db/references :schema]
         (get-in md)
         keys
         (candidates token))
    ))

(defn table-hint [connection op token]
  (let [results (connection/get-tables connection) ;; [ ["schema-a" "table-a"] ["schema-b" "table-b"] .. ]

        ;; TODO: filter the result set if there is a `context`. Bidirectional
        ;; references are required in the reference data to do that

        ;; Filter if schema is specified
        schema (:schema op)
        results (if schema (filter #(= (name schema) (first %1)) results) results)]
    (->> results
         (map second)
         (candidates token))))

(def hint-fns
  {:schema schema-hint
   :table table-hint})

(defn generate
  [connection op]
  (let [[token categories] (:partial op)]
    (into {}
          (for [c categories]
            (when-let [hint-fn (hint-fns c)]
              [c (hint-fn connection op token)])))))

