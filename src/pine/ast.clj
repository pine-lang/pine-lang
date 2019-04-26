(ns pine.ast
  (:require [clojure.string :as s]
            [pine.db :as db]
            [pine.utils :as utils]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            ))

;; Filter operations

(defn operation-type?
  "Check if the operation has one of the specified types"
  ([ts]
   (partial operation-type? ts))
  ([ts op]
   (letfn [(t? [t op] (= (:type op) t))]
     (some true?
           (map (utils/flip t? op) ts))

     )))


;; Parse

(def parse (let [dir (System/getProperty "user.dir")

                  file (format "%s/src/pine/pine.bnf" dir)
                 grammar (slurp file)]
             (insta/parser grammar)))

(defn add-context
  "Add the entity and the relevant alias which serves as the context for all the operations."
  [ops]
  (->> ops
   (reduce (fn [acc op]
             (let [prev-entity  (->> acc
                                     :context
                                     :entity)
                   prev-alias   (->> acc
                                     :context
                                     :alias)
                   curr-entity  (:entity op)
                   curr-alias   (:alias op)
                   next      (cond (operation-type? ["condition"] op) {:entity curr-entity :alias curr-alias}
                                   :else                              {:entity prev-entity :alias prev-alias})]
               {:context next
                :ops    (cons (assoc op :context {:entity prev-entity :alias prev-alias}) (:ops acc) )}
               )
             ) [])
   :ops
   reverse
   )
  )

(defn add-aliases
  "Add aliases to the the operations"
  [ops]
  (map-indexed (fn [idx op]
                 (cond (:entity op) (assoc op :alias (format "%s_%d" (name (:entity op)) idx))
                       :else op))
               ops))

;; Parse 'anything' comes from the parse tree
;; Indexed 'anything' is what we need int the AST
(defn str->operations
  "Create a filter AST from the raw query part"
  [expression]
(letfn [(get-parsed-ops [ops]
          (match ops
           [:OPERATIONS & parsed] parsed
           :else ops))
        (parsed-value->indexed-value [v]
          (match v
                 [:comparison
                  [:string column]
                  [:operator op ]
                  [:quoted-string [:space-string value]]]        [column [:string value]                                           op]
                 [:comparison
                  [:string column]
                  [:operator op ]
                  [:quoted-string ]]                             [column [:string ""]                                              op]
                 [:comparison
                  [:string column]
                  [:operator op ]
                  [:number value]]                               [column [:number value]                                           op]
                 [:comparison [:string column] [:ids & ids]]     [column [:expression (str "(" (s/join "," (map second ids)) ")")] "IN"]
                 [:comparison [:string column] "?" ]             [column [:expression "NULL"]                                      "IS NOT"]
                 [:comparison "!" [:string column] "?" ]         [column [:expression "NULL"]                                      "IS"]
                 [:assignment
                  [:string column]
                  [:quoted-string [:space-string value]]]        [column [:string value]                                           "="]
                 [:assignment
                  [:string column]
                  [:quoted-string ]]                             [column [:string ""]                                              "="]
                 [:assignment
                  [:string column]
                  [:number value]]                               [column [:number value]                                           "="]
                 :else                                           (throw (Exception. (format "Can't index value: %s" v)))
                 )
          )
        (parsed-cols->indexed-cols [cs]
          (reduce (fn [acc c]
                    (match c
                           [:column [:string column-name]]                           (conj acc column-name)
                           [:column [:string column-name] [:alias [:string a]]]      (conj acc (format "%s AS %s" column-name a))
                           :else (throw (Exception. (format "Can't index column: %s" c)))
                           )) [] cs)
          )
        (parsed-op->indexed-op [op]
          (match op
                 ;; resource/conditions
                 ;;
                 ;; At the moment the resource and condition operations are
                 ;; grouped
                 [:RESOURCE [:entity [:string table]] ]                   {:type "condition" :entity (keyword table) :values []}
                 [:RESOURCE [:entity [:string table]] [:id [:number id]]] {:type "condition" :entity (keyword table) :values [["id" [:number id] "="]]}
                 [:RESOURCE [:entity [:string table]] [:ids & ids]]       {:type "condition" :entity (keyword table) :values [["id" [:expression (str "(" (s/join "," (map second ids)) ")")] "IN"]]}
                 [:RESOURCE [:entity [:string table]] [:ands & values]]   {:type "condition" :entity (keyword table) :values (map parsed-value->indexed-value values) :or false}
                 [:RESOURCE [:entity [:string table]] [:ors & values]]    {:type "condition" :entity (keyword table) :values (map parsed-value->indexed-value values) :or true}
                 ;; select
                 [:SELECT [:specific [:columns & columns]]]               {:type "select" :columns (parsed-cols->indexed-cols columns)}
                 [:SELECT [:invert-specific [:columns & columns]]]        {:type "unselect" :columns (parsed-cols->indexed-cols columns)}
                 ;; limit
                 [:LIMIT [:number number]]                                {:type "limit" :count (Integer. number)}

                 ;; group
                 [:GROUP [:column [:string col]] ]                        {:type "group" :column col :fn-name "count" :fn-column col}
                 [:GROUP [:column [:string col-a]]
                  [:FUNCTION fn-name [:column [:string col-b]]]]          {:type "group" :column col-a :fn-name fn-name :fn-column col-b}

                 ;; function
                 [:FUNCTION fn-name [:column [:string col]]]              {:type "function" :fn-name fn-name :fn-column col}

                 ;; order
                 [:ORDER "+" [:column [:string column]]]                  {:type "order" :direction "ascending"  :column column}
                 [:ORDER [:column [:string column]]]                      {:type "order" :direction "descending" :column column}

                 ;; meta
                 [:META [:ref]]                                           {:type "meta" :fn-name "references"}

                 ;; Delete
                 [:DELETE]                                                {:type "delete"}

                 ;; Set
                 [:SET [:assignments & values] ]                          {:type "set" :values (map parsed-value->indexed-value values)}

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
  (->> expression         ;; users 1
       parse              ;; [ :OPERATIONS .. [:CONDITION .. ].. ]
       get-parsed-ops     ;; [ [:CONDITION ..                ] ]
       (map index)        ;; [ {:type "condition" ...} ]
       add-aliases        ;; [ {:type "condition" ... :entity :users :alias "u"} ]
       add-context        ;; [ [:type "condition" ... :context {:entity .. :alias ..} ] ]
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

(defn ast->sql-and-params-helper
  "Create an sql query"
  [ast]
  (let [select-columns (s/join ", " (ast :select))
        [table alias]  (ast :from)
        select-sql     (str "SELECT %s FROM %s AS %s")
        update-sql     (str "UPDATE %s AS %s")
        delete-table   (ast :delete)
        set-values     (ast :set)
        delete-sql     (str "DELETE FROM %s")
        where          (ast :where)
        order          (ast :order)
        group          (ast :group)
        limit          (ast :limit)
        joins          (ast :joins)
        join?          (not (empty? joins))
        conditions     (where :conditions)
        parameters (remove nil? (concat (cond set-values (set-values :params) :else []) (where :params)))
        ]

    [(apply format
            (->> [(cond set-values   update-sql
                        delete-table delete-sql
                        :else        select-sql)
                  (cond set-values "SET %s" :else nil)
                  (cond join? "%s" :else nil)
                  "WHERE %s"
                  order
                  group
                  limit
                  ]
                 (remove nil?)
                 (s/join " ")
                 )

            (->> [(cond set-values   nil
                        delete-table nil
                        :else        select-columns)                   ;; update/delete/select
                  (name table)                                         ;; table
                  (cond set-values   alias
                        delete-table nil
                        :else        alias)                            ;; alias
                  (cond set-values  (s/join ", " (set-values :values))
                        :else        nil)                              ;; set
                  (cond join? (ast-joins->sql joins) :else nil)        ;; joins
                  (s/join " AND " conditions)                          ;; where
                  ]
                 (remove nil?))
            )
     parameters]
    ))

(defn ast->sql-and-params
  "Create an sql query from the ast."
  [ast]
  (let [meta (:meta ast)]
    (cond meta [meta []]
          :else (ast->sql-and-params-helper ast))))

;; -----------------
;; DB operations
;; -----------------

(defn table-alias
  "Alias for a table. This function needs a better name. This is more like a reference to the most relevant `condition` operation. Maybe this should be called `reference`"
  [op]
  ;; Use the table name as the alias
  (or (:alias op) (-> op :context :alias))
  ;; Create an alias from the camel case name
  ;; (let [t (name table)]
  ;;   (str
  ;;    (str (first t))
  ;;    (s/lower-case (or (apply str (re-seq #"[A-Z]" t)) ""))))
  )

(defn qualify
  "Qualify a column with the alias: (qualify \"caseFileId\" :with \"d\") => \"d.caseFileId\""
  [column _ alias]
  (format "%s.%s" alias column)
  )


(defn primary-key
  "Get the qualified primary key for the table. This is a naive function that
  assumes that the primary key is always Id."
  [op]
  (format "%s.%s" (op :alias) "id"))

;; -----------------
;; Operations to AST
;; -----------------

(defn alias->select-all
  "Get the columns for the last 'condition' operation or the select operation"
  [alias]
  [(format "%s.*" alias)])

(defn pine-fn->sql-fn
  "Get the relevant SQL function to be used"
  [fn-name]
  (case fn-name
    "join" "GROUP_CONCAT"
    fn-name
    )
  )

(defn operation->function-columns
  "Generate the sql for the 'function' operation"
  [op]
  (let [sql-fn-name (->> op :fn-name pine-fn->sql-fn)
        fn-column   (op :fn-column)
        alias       (->> op
                         :context
                         :alias)]
    ;; @todo: return a vector instead of a string with all the columns
    (format "%s(%s.%s)" sql-fn-name alias fn-column)))

(defn operation->group-columns
  "Generate the sql for the 'group' operation"
  [op]
  (let [
        sql-fn-name (->> op :fn-name pine-fn->sql-fn)
        column      (op :column)
        fn-column   (op :fn-column)
        alias       (->> op
                         :context
                         :alias)]
    ;; @todo: return a vector instead of a string with all the columns
    (format "%s.%s, %s(%s.%s)" alias column sql-fn-name alias fn-column)))

(defn operations->group-columns
  "Get the columns for the last 'condition' operation or the select operation"
  [ops]
  (map operation->group-columns ops))


(defn operations->select-specific
  "Get the columns specified using the 'select:' keyword"
  [schema ops]
  (->> ops
       (filter (operation-type? ["condition", "select"]))
       (partition 2 1)
       (filter (fn [[a b]] (and (operation-type? ["condition"] a)
                               (operation-type? ["select"] b))))
       (map (fn [pair] (let [a       (table-alias (first pair))
                             columns (:columns (second pair))]
                         (map (fn [c] (format "%s.%s" a c)) columns)
                         )))
       (apply concat)
       )
  )

(defn expand-signle-column
  "Expands table.* selections if they match the operation"
  [schema column op]
  (if
   (= column (format "%s.*" (:alias (:context op))))
     (->>
       (:entity (:context op))
       (db/get-columns schema)
       (map #(format "%s.%s" (:alias (:context op)) %)))
     [column]))

(defn expand-columns-if-operated-on
  "Expands 'table.*' selections if they are related to operations"
  [schema columns ops]
  (match [columns ops]
         [some-columns                       ([]  :seq)] some-columns
         [[first-column & rest-of-columns]   ([first-op & rest-of-ops] :seq)]
           (let [expanded       (expand-signle-column schema first-column first-op)
                 columns-so-far (reduce conj (vec expanded) rest-of-columns)]
             (expand-columns-if-operated-on schema columns-so-far rest-of-ops))))

(defn operation->exclude-columns
  "Removes unselected columns from results"
  [schema columns unselect-ops]

  (let [expanded-columns (expand-columns-if-operated-on schema (vec columns) unselect-ops)
        excluded-columns (mapcat
             (fn [op] (map
                        (fn [column] (format "%s.%s" (:alias (:context op)) column))
                        (:columns op)))
             unselect-ops)
        ]
    (remove (set excluded-columns) expanded-columns))
  )

(defn operations->select-columns
  "Get the columns for the last 'condition' operation or the select operation"
  [schema operations]
  (let [specific-columns    (operations->select-specific schema operations)
        group-columns       (->> operations
                                 (filter (operation-type? ["group"]))
                                 operations->group-columns
                                 )
        all-columns-needed?  (->> operations
                                  (filter (operation-type? ["select", "condition"]))
                                  last
                                  (operation-type? ["condition"]))
        all-columns          (cond all-columns-needed? (->> operations
                                                            last
                                                            table-alias
                                                            alias->select-all
                                                            )
                                   :else               [])
        function-columns-needed? (->> operations
                                      last
                                      (operation-type? ["function"]))
        function-columns          (cond function-columns-needed? (->> operations
                                                            last
                                                            operation->function-columns
                                                            vector
                                                            )
                                   :else               [])
        ]

    ;; If function columns are needed, ignore everything else and just show them, otherwise following the table:
    ;;
    ;; | specific columns | group columns | SELECTED            |
    ;; | ✓                | ✓             | specific + function |
    ;; | ✓                | x             | specific + all      |
    ;; | x                | ✓             | function            |
    ;; | x                | x             | all                 |

    (let [columns-before-exclusion (cond
            (not-empty function-columns)                                 function-columns
            (and (not-empty specific-columns) (not-empty group-columns)) (concat specific-columns group-columns)
            (and (not-empty specific-columns) (empty? group-columns))    (concat specific-columns all-columns)
            (and (empty? specific-columns)    (not-empty group-columns)) group-columns
            (and (empty? specific-columns)    (empty? group-columns))    all-columns
            :else                                                           ["?"]
          )
          columns-after-exclusion (->> operations
                                       (filter (operation-type? ["unselect"]))
                                       (operation->exclude-columns schema columns-before-exclusion)
                                       )
        ]

      columns-after-exclusion
      )
    )
  )


(defn operations->join
  "Get the join from 2 operations"
  [schema o1 o2]
  (let [e1  (:entity o1)
        e2  (:entity o2)
        a1 (table-alias o1)
        a2 (table-alias o2)
        e1-foreign-key (db/relation schema e2 :owns e1)
        e2-foreign-key (db/relation schema e1 :owns e2)
        join-on (cond e1-foreign-key [(primary-key o2) (qualify e1-foreign-key :with a1)]
                      e2-foreign-key [(qualify e2-foreign-key :with a2) (primary-key o1)]
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
  [entity qualify? [column [type value] op]]
  (let [col (cond qualify? (qualify column :with (name entity)) :else column)
        operator (cond (= :null value) op
                       (re-find #"\*" value) "LIKE"
                       :else op)
        val      (case value
                   :null :null
                   (s/replace value "*" "%")
                   )]
    (case val
      :null [(format "%s %s" col operator)]
      [(format "%s %s ?" col operator) [type val]]
      )
    )
  )

(defn operation->where
  "Get the where condition for an operation."
  [qualify? operation]
  (let [vs (:values operation)
        or (:or operation)]
    (cond (empty? vs) { :conditions "1" :params nil}
          :else       (->> vs
                           (map (partial filter->where-condition (:alias operation) qualify?))
                           ((fn [where-conditions] { :conditions (str "(" (->> where-conditions
                                                                               (map first)
                                                                               (s/join (cond or " OR " :else " AND "))) ")")
                                                    :params (->> where-conditions
                                                         (map second)
                                                         vec
                                                         ) }))

                           ))))

(defn operations->where
  "Get the joins from the operations"
  [qualify? ops]
  (let [
        wheres (map (partial operation->where qualify?) ops)
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
  (let [delete? (->> ops
                     (filter (operation-type? ["delete"]))
                     count
                     (#(> %1 0))
                     )]
    (->> ops
         (filter (operation-type? ["limit"]))
         last
         ((fn [op] (format "LIMIT %s" (or (:count op) (cond delete? 1 :else 50)))))
         ))
  )

(defn operations->order
  "Create the ORDER BY SQL statement"
  [ops]
  (let [[condition-op order-op] (->> ops
                                     (filter (operation-type? ["condition", "order"]))
                                     (partition 2 1)
                                     (filter (fn [ops] (and (operation-type? ["condition"] (first ops))
                                                            (operation-type? ["order"] (second ops))
                                                          )))
                                     (last)
                                     )
        ]
    (cond (and condition-op order-op) (let [direction (order-op :direction)
                                            column (order-op :column)
                                            a      (table-alias condition-op)
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
                                     (filter (operation-type? ["condition", "group"]))
                                     (partition 2 1)
                                     (filter (fn [ops] (and (operation-type? ["condition"] (first ops))
                                                            (operation-type? ["group"] (second ops))
                                                          )))
                                     (last)
                                     )
        ]
    (cond (and condition-op group-op) (let [

                                         column (group-op :column)
                                         a      (table-alias condition-op)

                                         ]
                                     (format "GROUP BY %s.%s" a column)
                                     )
          :else nil
          )
    ))

(defn operations->meta
  "Execute the command related to the meta function"
  [ops]
  (cond (operation-type? ["meta"] (last ops)) (let [[condition-op meta-op]
                                                    (->> ops
                                                                                  (filter (operation-type? ["condition", "meta"]))
                                                                                  (partition 2 1)
                                                                                  (filter (fn [ops] (and (operation-type? ["condition"] (first ops))
                                                                                                         (operation-type? ["meta"] (second ops))
                                                                                                         )))
                                                                                  (last)
                                                                                  )
                                                      ]
                                                  (cond (and condition-op meta-op) (let [
                                                                                          fn-name (meta-op :fn-name)
                                                                                          entity (:entity condition-op)
                                                                                          ]
                                                                                      (format "show create table %s" (name entity))
                                                                                      )
                                                        :else nil
                                                        )
                                                  )
        :else nil
        )
  )

(defn operations->delete
  "Execute the command related to the meta function"
  [ops]
  (cond (operation-type? ["delete"] (last ops)) (let [[condition-op delete-op]
                                                    (->> ops
                                                                                  (filter (operation-type? ["condition", "delete"]))
                                                                                  (partition 2 1)
                                                                                  (filter (fn [ops] (and (operation-type? ["condition"] (first ops))
                                                                                                         (operation-type? ["delete"] (second ops))
                                                                                                         )))
                                                                                  (last)
                                                                                  )
                                                      ]
                                                  (cond (and condition-op delete-op) (let [entity (:entity condition-op)]
                                                                                       (name entity))
                                                        :else nil
                                                        )
                                                  )
        :else nil
        )
  )

(defn operation->set
  "Execute the command "
  [op]
  (let [values (op :values)
        alias  (table-alias op)
        ]
    {
     :values (->> values
                  (map first)
                  (map (fn [column]
                         (str (qualify column :with alias) " = ?"))))
     :params (->> values
                  (map second)
                  vec)
     }))

(defn operations->set
  "Create the AST for the SET part"
  [operations]
  (let [ops (filter (operation-type? ["set"]) operations)]
    (cond (empty? ops) nil
          :else        (->> ops
                            last
                            operation->set)))
  )

(defn operations->ast
  "operations to ast"
  [schema ops]
  (let [columns       (operations->select-columns schema ops)
        condition-ops (filter (operation-type? ["condition"]) ops)
        primary-op    (first condition-ops)
        table         (:entity primary-op)
        delete        (operations->delete ops)
        set-values    (operations->set ops)
        joins         (operations->joins schema condition-ops)
        where         (operations->where (cond delete nil :else true) condition-ops)
        order         (operations->order ops)
        group         (operations->group ops)
        limit         (operations->limit ops)
        meta          (operations->meta ops)
        ]
    {
     :select columns
     :from [table (table-alias primary-op)]
     :joins joins
     :where where
     :order order
     :group group
     :limit limit
     :meta  meta
     :delete delete
     :set    set-values
     }
    )
  )
