(ns pine.hints
  (:require [clojure.string :as s]
            [pine.db.protocol :as protocol]
            [pine.db :as db]
            [clojure.core.match :refer [match]]))

(defn- abbreviate [x]
  (let [parts (s/split x #"_")
        initials (map first parts)]
    (s/lower-case (s/join "" initials))))

(defn candidates [token candidates]
  (let [abbreviations (map abbreviate candidates)]
    (set
     (concat (filter #(s/includes? (s/lower-case %1) token) candidates)
             (filter #(s/includes? (abbreviate %1) token) candidates)))))

(defn schema-hint [op token]
  (let [result (protocol/get-tables @db/connection) ;; [ ["schema-a" "table-a"] ["schema-b" "table-b"] .. ]
        schemas (map first result)                  ;; [ ["schema-a"] ["schema-b"] .. ]
        cs (candidates token schemas)
        ]
    cs
    ))

(defn table-hint [op token]
  (let [results (protocol/get-tables @db/connection) ;; [ ["schema-a" "table-a"] ["schema-b" "table-b"] .. ]

        ;; TODO: filter the result set if there is a `context`. Bidirectional
        ;; references are required in the reference data to do that

        ;; Filter if schema is specified
        schema (:schema op)
        results (if schema (filter #(= (name schema) (first %1)) results) results)
        ]
    (->> results
         (map second)
         (candidates token))
    ))

(def hint-fns
  {:schema schema-hint
   :table table-hint})

(defn generate
  [op]
  (let [[token categories] (:partial op)]
    (into {}
          (for [c categories]
            (when-let [hint-fn (hint-fns c)]
              [c (hint-fn op token)])))))

