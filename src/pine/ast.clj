(ns pine.ast
  (:require [clojure.string :as s])
  )

;; Parse

(defn str->filter
  "Create a filter AST from the raw query part"
  [x]
  (cond->> x
    (re-matches #"[^0-9]+" x) (hash-map :name)
    (re-matches #"[0-9]+" x) (hash-map :id)
    ))

(defn str->operations
  "Get the operations from the query. An operation is a single executable atom."
  [query]
  (->>
   (s/split query #"\s*\|\s*")
   (map #(s/split %1 #"\s+"))
   ;; (map #(hash-map (keyword (first %1)) (str->filter (second %1))))
   (map #(hash-map
          :entity (keyword (first %1))
          :filter (str->filter (second %1))))
   (vec)
   ))


;; Build SQL from the AST


(defn table->sql
  "Table to sql"
  [table]
  (str "SELECT * FROM " (name table)))


;; TODO: escape
(defn filter->sql
  "Filter to SQL"
  [filter]
  (cond
    (contains? filter :id) (str "AND id = " (:id filter))
    (contains? filter :name) (str "AND name = \"\"" (:name filter) "\"")
    )
  )

(defn ast->sql
  "Create an sql query"
  [ast]

  (loop [ops ast acc ""]
    (if (empty? ops) acc
        (recur (rest ops)
               (let [op (first ops)
                     table (:entity op)
                     filter (:filter op)
                     prefix (if (empty? acc)
                              (s/join " " [(table->sql table)
                                           "WHERE 1"])
                                "")]
                 (s/join " "
                         [prefix
                          acc
                          (filter->sql filter)])
                 )
               )))
  )

;; ------------
;; Common Utils
;; ------------

(defn singular
  "Drop the s at the end of the word"
  [s]
  (cond (re-matches #".*s$" s) (s/join "" (drop-last s))
        :else s)
  )

;; -----------------
;; DB operations
;; -----------------

(defn primary-key
  "Get the qualified primary key for the table. This is a naive function that
  assumes that the primary key is always Id."
  [table]
  (let [t (name table)]
    (str (alias t) ".id")
    ))

(defn foreign-key
  "Get the qualified foreign key for the table. This is a naive function that
  tries to guess the foreign key instead of looking at the schema."
  [table foreign-table]
  (let [t (name table)
        ft (name foreign-table)]
    (str (alias t) "." (singular ft) "Id")
    ))

(defn alias
  "Alias for a table. At some point, fix this so that it also works for snake case
  strings."
  [table]
  (let [t (name table)]
    (str
     (str (first t))
     (s/lower-case (or (apply str (re-seq #"[A-Z]" t)) ""))))
  )

;; -----------------
;; Operations to AST
;; -----------------


(defn operations->primary-table
  "Get the primary table from the operations"
  [operations]
  (->> operations
       first
       :entity))

(defn operations->join
  "Get the join from 2 operatoins"
  [o1 o2]
  (let [
        entity-1  (:entity o1)
        entity-2  (:entity o2)
        alias-2 (alias entity-2)
        alias-1 (alias entity-1)
        ]
    [entity-2 alias-2 [(foreign-key entity-2 entity-1) (primary-key entity-1)]])
  )

(defn operations->joins
  "Get the joins from the operations"
  [operations]
  (let [[op & ops] operations]
    (cond (nil? ops) []
          :else (reduce (fn [acc [o1 o2]]
                          (concat acc (operations->join o1 o2))
                          )
                        [] (partition 2 1 operations)))
    )
  )

(defn operations->ast
  "operations to ast"
  [operations]
  (reduce (fn [ast operation]
            (letfn [
                    (set-table [ast]
                      (cond (:from ast) ast
                            :else (update ast :from (fn [_] (:entity operation)))))
                    ]
              (->> ast
                  )

              )

            )
          {:from (operations->primary-table operations)} operations)
  )


(operations->ast operations)

(def operations
  [{:entity :customers, :filter {:id "1"}}
   {:entity :users, :filter {:id "2"}}
   {:entity :address, :filter {:name "test"}}
   ])


(:from {})

(cond false 1 :else 2)

(update {:a 1 :b 2} :join (fn [x] 123))

(update-in {:a {} :b 2} [:a :join] (fn [x] 123))

;; (let [tables (map :entity ast)]
;;   (loop [table  (first tables)
;;          ts (rest tables)
;;          acc (str " FROM " (name table))]
;;     (if (empty? ts) acc
;;         (let [t1 table
;;               t2 (first ts)]
;;           (recur t2 (rest ts) (str acc " JOIN " (name t2) " ON " " something = something")))))
;;   )

(reduce (fn [acc x] (+ acc x)) 10 [1 2 3 4 5])



(operations->joins-ast
 [{:entity :customers, :filter {:id "1"}}
  {:entity :users, :filter {:id "2"}}])
