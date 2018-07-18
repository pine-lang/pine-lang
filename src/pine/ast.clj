(ns pine.ast
  (:require [clojure.string :as s])
  )

;; Parse

(defn str->filter
  "Create a filter AST from the raw query part"
  [x]
  (cond (= "*" x) {}
        :else (cond->> x
                (re-matches #"[^0-9]+" x) (hash-map :name)
                (re-matches #"[0-9]+" x) (hash-map :id)
                )))

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


(defn ast-join->sql
  "Convert a single join in the joins parts of the AST to an sql query"
  [entity alias [t1 t2]]
  (format "JOIN %s AS %s ON (%s = %s)" (name entity) alias t1 t2) )

(defn ast-joins->sql
  "Convert joins part of the AST to an sql query"
  [joins]
  (->> joins
       (partition 3 3)
       (map (partial apply ast-join->sql))
       (s/join " ")
       )
  )

(defn ast->sql-and-params
  "Create an sql query"
  [ast]
  (let [select (s/join ", " (ast :select))
        [table alias] (ast :from)
        where (ast :where)
        joins (ast :joins)
        join? (not (empty? joins))
        conditions (where :conditions)
        parameters (where :params)
        ]

    [(apply format
            (->> ["SELECT %s"
                  "FROM %s AS %s"
                  (cond join? "%s" :else nil)
                  "WHERE %s"
                  "LIMIT 10"]
                 (remove nil?)
                 (s/join " ")
                 )

            (->> [select                                        ;; select
                  (name table) alias                            ;; from
                  (cond join? (ast-joins->sql joins) :else nil) ;; joins
                  (s/join " AND " conditions)                   ;; where
                  ]
                 (remove nil?))
            )
     parameters]
    ))

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

(defn alias
  "Alias for a table. At some point, fix this so that it also works for snake case
  strings."
  [table]
  (let [t (name table)]
    (str
     (str (first t))
     (s/lower-case (or (apply str (re-seq #"[A-Z]" t)) ""))))
  )

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


;; -----------------
;; Operations to AST
;; -----------------


(defn operations->primary-table
  "Get the primary table from the operations"
  [schema operations]
  (->> operations
       first
       :entity))

(defn operations->select-columns
  "Get the columns for the last operation"
  [schema operations]
  (let [op (last operations)
        entity (op :entity)]
    [(format "%s.*" (alias entity))]))

(defn operations->join
  "Get the join from 2 operatoins"
  [schema o1 o2]
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
  [schema operations]
  (let [[op & ops] operations]
    (->>
     (cond (nil? ops) []
           :else (reduce (fn [acc [o1 o2]]
                           (concat acc (operations->join schema o1 o2))
                           )
                         [] (partition 2 1 operations)))
     (apply vector)
     )
    )
  )

(defn operation->where
  "Get the where condition for an operation."
  [schema operation]
  (let [filter (:filter operation)
        entity (:entity operation)
        a      (alias entity)
        ]
    (cond (:id filter) [(str a ".id = ?" ) (:id filter)]
          (:name filter) [(str a ".name LIKE ?") (str (:name filter) "%")]
          :else ["1" nil]
          )
    )
  )

(defn operations->where
  "Get the joins from the operations"
  [schema ops]
  (let [
        where (map (partial operation->where schema) ops)
        [conditions params] (apply (partial map vector) where)
        ]
    {
     :conditions conditions
     :params     (vec (remove nil? params))
     }
    )
  )

(defn operations->ast
  "operations to ast"
  [schema ops]
  (let [columns (operations->select-columns schema ops)
        table (operations->primary-table schema ops)
        joins (operations->joins schema ops)
        where (operations->where schema ops)]
    {
     :select columns
     :from [table (alias table)]
     :joins joins
     :where where
     }
    )
  )
