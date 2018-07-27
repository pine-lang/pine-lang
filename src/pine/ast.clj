(ns pine.ast
  (:require [clojure.string :as s]
            [pine.db :as db]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            ))

(def parse (let [dir (System/getProperty "user.dir")
                  file (format "%s/src/pine/pine.bnf" dir)
                 grammar (slurp file)]
             (insta/parser grammar)))

;; (parse "users * | caseFiles *")

;; (str->operations "users * | caseFiles *")

;; Parse

(defn str->operations
  "Create a filter AST from the raw query part"
  [expression]
(letfn [(get-parsed-ops [ops]
          (match ops
           [:OPERATIONS & parsed] parsed
           :else ops))
        (parsed-filter->indexed-filter [f]
          (match f
                 [:filter [:implicit-filter [:string "*"]]]                    []
                 [:filter [:implicit-filter [:string value]]]                  ["id" value]
                 [:filter [:explicit-filter [:string column] [:string value]]] [column value]
                 :else (throw (Exception. "Can't index filter"))
                 )
          )
        (parsed-op->indexed-op [op]
          (match op
                 [:condition [:entity [:string table]] [:filters & filters]]   {:entity (keyword table) :filter (parsed-filter->indexed-filter (first filters))}
                 :else (throw (Exception. "Can't convert format of operation"))
                 )
          )
        (index [op]
          (match op
                 [:operation o] (parsed-op->indexed-op o)
                 :else (throw (Exception. "Can't index filter"))
                 ))
        ]
  (->> expression
       parse
       get-parsed-ops
       (map index)
       )))


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
                  "LIMIT 50"]
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

;; -----------------
;; DB operations
;; -----------------

(defn table-alias
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
    (str (table-alias t) ".id")
    ))

(defn qualify
  "Qualify a column with the alias: (qualify \"caseFileId\" :with \"d\") => \"d.caseFileId\""
  [column _ alias]
  (format "%s.%s" alias column)
  )

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
    [(format "%s.*" (table-alias entity))])
  )

(defn operations->join
  "Get the join from 2 operations"
  [schema o1 o2]
  (let [e1  (:entity o1)
        e2  (:entity o2)
        a1 (table-alias e1)
        a2 (table-alias e2)
        e1-foreign-key (db/relation schema e2 :owns e1)
        e2-foreign-key (db/relation schema e1 :owns e2)
        join-on (cond e1-foreign-key [(primary-key e2) (qualify e1-foreign-key :with (table-alias e1))]
                      e2-foreign-key [(qualify e2-foreign-key :with (table-alias e2)) (primary-key e1)]
                      :else ["1" "2 /* No relationship exists */"])
        ]

    [e2 a2 join-on]
    ))



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
  (let [filter (:filter operation)]
    (cond (empty? filter) ["1" nil]
          :else           (let [[column value] filter
                                entity (:entity operation)
                                a      (table-alias entity)
                                operator (cond (re-find #"\*" value) "LIKE" :else "=")
                                val      (s/replace value "*" "%")]
                            [(format "%s.%s %s ?" a column operator) val]))))

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
     :from [table (table-alias table)]
     :joins joins
     :where where
     }
    )
  )
