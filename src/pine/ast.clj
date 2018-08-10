(ns pine.ast
  (:require [clojure.string :as s]
            [pine.db :as db]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [pine.fixtures :as fixtures]))

(def parse (let [dir (System/getProperty "user.dir")
                  file (format "%s/src/pine/pine.bnf" dir)
                 grammar (slurp file)]
             (insta/parser grammar)))

;; (str->operations "users * | caseFiles *")

;; Parse

(defn operation-type?
  "Check the type of the operation"
  [type operation]
  (= (:type operation) type))

(defn filter-operations
  "Filter out the type of operations where type can be: select, condition"
  [name ops]
  (filter (partial operation-type? name) ops))

(defn str->operations
  "Create a filter AST from the raw query part"
  [expression]
(letfn [(get-parsed-ops [ops]
          (match ops
           [:OPERATIONS & parsed] parsed
           :else ops))
        (parsed-filter->indexed-filter [f]
          (match f
                 [:filter [:implicit-filter [:string value]]]                  ["id" value]
                 [:filter [:explicit-filter [:string column] [:string value]]] [column value]
                 :else (throw (Exception. "Can't index filter"))
                 )
          )
        (parsed-cols->indexed-cols [cs]
          (reduce (fn [acc c]
                    (match c
                           [:column [:string column-name]] (conj acc column-name)
                           :else (throw (Exception. (format "Can't index column: %s" c)))
                           )) [] cs)
          )
        (parsed-op->indexed-op [op]
          (match op
                 ;; conditions
                 [:CONDITION [:entity [:string table]] ]                       {:type "condition" :entity (keyword table) :filters []}
                 [:CONDITION [:entity [:string table]] [:filters & filters]]   {:type "condition" :entity (keyword table) :filters (map parsed-filter->indexed-filter filters)}
                 ;; select
                 [:SELECT [:specific [:columns & columns]]]                    {:type "select" :columns (parsed-cols->indexed-cols columns)}
                 [:SELECT [:invert-specific [:columns & columns]]]             (throw (Exception. "Unselect key word is not supported yet."))
                 ;; limit
                 [:LIMIT [:number number]]                                     {:type "limit" :count (Integer. number)}
                 ;; function
                 [:FUNCTION fn-name [:columns [:column [:string column]]]]     {:type "function" :fn-name fn-name :columns [column]}

                 ;; group
                 [:GROUP [:columns [:column [:string column]]]]                {:type "group" :columns [column]}

                 ;; order
                 [:ORDER "+" [:column [:string column]]]                       {:type "order" :direction "ascending"  :column column}
                 [:ORDER [:column [:string column]]]                           {:type "order" :direction "descending" :column column}
                 ;; not specified
                 :else (throw (Exception. (format "Can't convert format of operation: %s" op)))
                 )
          )
        (index [op]
          (match op
                 [:OPERATION o] (parsed-op->indexed-op o)
                 :else (throw (Exception. (format  "Can't index operation: %s" op)))
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
        group (ast :group)
        limit (ast :limit)
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
                  group
                  limit
                  ]
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

(defn operation->select-all
  "Get the columns for the last 'condition' operation or the select operation"
  [schema op]
  (let [entity (op :entity)]
    [(format "%s.*" (table-alias entity))]))

(defn operations->function-columns
  "Get the columns for the last 'condition' operation or the select operation"
  [ops]
  (let [[condition-op function-op] (->> ops
                                    (filter (fn [op] (or (operation-type? "condition" op)
                                                         (operation-type? "function" op)
                                                         )))
                                    (partition 2 1)
                                    (filter (fn [ops] (and (operation-type? "condition" (first ops))
                                                           (operation-type? "function" (second ops))
                                                           )))
                                    last
                                )
        fn-name (function-op :fn-name)
        columns (function-op :columns)
        entity (:entity condition-op)
        a      (table-alias entity)

        ]
    [(format "%s(%s.%s)" fn-name a (first columns))]))


(defn operations->select-specific
  "Get the columns specified using the 'select:' keyword"
  [schema ops]
  (->> ops
       (filter (fn [op] (or (operation-type? "condition" op)
                            (operation-type? "select" op)
                            )))
       (partition 2 1)
       (filter (fn [[a b]] (and (operation-type? "condition" a)
                               (operation-type? "select" b))))
       (map (fn [pair] (let [
                             entity (:entity (first pair))
                             a      (table-alias entity)
                             columns (:columns (second pair))]
                         (map (fn [c] (format "%s.%s" a c)) columns)
                         )))
       (apply concat)
       )
  )

(defn operations->select-columns
  "Get the columns for the last 'condition' operation or the select operation"
  [schema operations]
  (let [specific-columns (operations->select-specific schema operations)
        condition-ops    (filter (partial operation-type? "condition") operations)
        function-columns (let [pairs        (->> operations
                                                 (filter (fn [op] (or (operation-type? "condition" op)
                                                                      (operation-type? "function" op))))
                                                 (partition 2 1)
                                                 (filter (fn [[a b]] (and (operation-type? "condition" a)
                                                                          (operation-type? "function" b)))))
                               relevant-ops (cond (empty? pairs) []
                                                  :else (->> pairs
                                                             ((fn [pairs]
                                                                (concat
                                                                 (->> pairs
                                                                      (map first))
                                                                 [(second (last pairs))]
                                                                 )))))]
                           (cond (empty? relevant-ops) []
                                 :else (operations->function-columns relevant-ops)))
        all-columns      (->> operations
                              (filter (fn [op] (or (operation-type? "select" op)
                                                   (operation-type? "condition" op))))

                              last
                              ((fn [op] (cond (operation-type? "condition" op) (operation->select-all schema op)
                                              :else [])))
                              )]

    ;; only show all the columns if function columns are not specified
    ;;
    ;; | specific columns | function columns | SELECTED            |
    ;; | x                | x                | specific + function |
    ;; | x                | o                | specific + all      |
    ;; | o                | x                | function            |
    ;; | o                | o                | all                 |

    (cond (and (not-empty specific-columns) (not-empty function-columns)) (concat specific-columns function-columns)
          (and (not-empty specific-columns) (empty? function-columns))    (concat specific-columns all-columns)
          (and (empty? specific-columns)    (not-empty function-columns)) function-columns
          (and (empty? specific-columns)    (empty? function-columns))    all-columns
          :else                                                           ["?"]
          )
    )
  )

;; (pine/$prepare fixtures/schema "caseFiles 1 | l: 1 | count: status | s: id" )


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

(defn filter->where-condition
  "Convert the filter part of an operation to a where sql"
  [entity [column value]]
  (let [a      (table-alias entity)
        operator (cond (re-find #"\*" value) "LIKE" :else "=")
        val      (s/replace value "*" "%")]
    [(format "%s.%s %s ?" a column operator) val])
  )

(defn operation->where
  "Get the where condition for an operation."
  [schema operation]
  (let [fs (:filters operation)]
    (cond (empty? fs) { :conditions "1" :params nil}
          :else       (->> fs
                           (map (partial filter->where-condition (:entity operation)))
                           ((fn [where-conditions] { :conditions (->> where-conditions
                                                         (map first)
                                                         (s/join " AND ")
                                                         )
                                                    :params (->> where-conditions
                                                         (map second)
                                                         vec
                                                         ) }))

                           ))))

(defn operations->where
  "Get the joins from the operations"
  [schema ops]
  (let [
        wheres (map (partial operation->where schema) ops)
        ]
    {
     :conditions (->> wheres
                     (map :conditions)
                     )
     :params (->> wheres
                  (map :params)
                  (apply concat)
                  vec
                  )
     }
    )
  )

(defn operations->limit
  "Get the joins from the operations"
  [ops]
  (->> ops
       (filter-operations "limit")
       last
       ((fn [op] (format "LIMIT %s" (or (:count op) 50))))
       )
  )

(defn operations->group
  "Create the ORDER BY SQL statement"
  [ops]
  (let [[condition-op order-op] (->> ops
                                     (filter (fn [op] (or (operation-type? "condition" op)
                                                          (operation-type? "order" op)
                                                          )))
                                     (partition 2 1)
                                     (filter (fn [ops] (and (operation-type? "condition" (first ops))
                                                          (operation-type? "order" (second ops))
                                                          )))
                                     (last)
                                     )
        ]
    (cond (and condition-op order-op) (let [direction (order-op :direction)
                                            column (order-op :column)
                                            entity (:entity condition-op)
                                            a      (table-alias entity)
                                         ]
                                     (format "ORDER BY %s.%s %s" a column (cond (= direction "ascending") "ASC" :else "DESC"))
                                     )
          :else nil
          )
    ))

(defn operations->group
  "Create the GROUP BY SQL statement"
  [ops]
  (let [[condition-op group-op] (->> ops
                                     (filter (fn [op] (or (operation-type? "condition" op)
                                                          (operation-type? "group" op)
                                                          )))
                                     (partition 2 1)
                                     (filter (fn [ops] (and (operation-type? "condition" (first ops))
                                                          (operation-type? "group" (second ops))
                                                          )))
                                     (last)
                                     )
        ]
    (cond (and condition-op group-op) (let [

                                         columns (group-op :columns)
                                         entity (:entity condition-op)
                                         a      (table-alias entity)

                                         ]
                                     (format "GROUP BY %s.%s" a (first columns))
                                     )
          :else nil
          )
    ))

;; (str->operations "users * | l: 1 | count: test")

(defn operations->ast
  "operations to ast"
  [schema ops]
  (let [columns (operations->select-columns schema ops)
        condition-ops (filter-operations "condition" ops)
        table (operations->primary-table schema condition-ops)
        joins (operations->joins schema condition-ops)
        where (operations->where schema condition-ops)
        order (operations->order ops)
        group (operations->group ops)
        limit (operations->limit ops)
        ]
    {
     :select columns
     :from [table (table-alias table)]
     :joins joins
     :where where
     :group group
     :limit limit
     }
    )
  )
