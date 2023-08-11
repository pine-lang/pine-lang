(ns pine.ast
  (:require [clojure.string :as s]
            [pine.db :as db]
            [pine.db.connection :as connection]
            [pine.state :as state]
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
                 (cond (:entity op) (assoc op :alias (format "%s_%d" (s/lower-case (name (:entity op))) idx))
                       :else op))
               ops))

;; Parse 'anything' comes from the parse tree
;; Indexed 'anything' is what we need int the AST
(defn str->operations
  "Create a filter AST from the raw query part"
  [connection expression]
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
                              [:column [:string column-name]]                           (conj acc (connection/quote connection column-name))
                              [:column [:string column-name] [:alias [:string a]]]      (conj acc (format "%s AS %s" (connection/quote connection column-name) a))
                              :else (throw (Exception. (format "Can't index column: %s" c)))
                              )) [] cs)
             )
           (parsed-op->indexed-op [op]
             (match op
                    ;; resource/conditions
                    [:RESOURCE [:entity                 [:partial-token token]] ]                   {:type "condition"                          :entity (keyword token) :partial [token [:schema :table]] :values []}
                    [:RESOURCE [:entity [:token schema] [:partial-token token]] ]                   {:type "condition" :schema (keyword schema) :entity (keyword token) :partial [token [        :table]] :values []}
                    [:RESOURCE [:entity                 [:partial-token token]] [:id [:number id]]] {:type "condition"                          :entity (keyword token) :partial [token [:schema :table]] :values [["id" [:number id] "="]]}
                    [:RESOURCE [:entity [:token schema] [:partial-token token]] [:id [:number id]]] {:type "condition" :schema (keyword schema) :entity (keyword token) :partial [token [        :table]] :values [["id" [:number id] "="]]}
                    [:RESOURCE [:entity                 [:partial-token token]] [:ids & ids]]       {:type "condition"                          :entity (keyword token) :partial [token [:schema :table]] :values [["id" [:expression (str "(" (s/join "," (map second ids)) ")")] "IN"]]}
                    [:RESOURCE [:entity [:token schema] [:partial-token token]] [:ids & ids]]       {:type "condition" :schema (keyword schema) :entity (keyword token) :partial [token [        :table]] :values [["id" [:expression (str "(" (s/join "," (map second ids)) ")")] "IN"]]}
                    [:RESOURCE [:entity                 [:partial-token token]] [:ands & values]]   {:type "condition"                          :entity (keyword token) :partial [token [:schema :table]] :values (map parsed-value->indexed-value values) :or false}
                    [:RESOURCE [:entity [:token schema] [:partial-token token]] [:ands & values]]   {:type "condition" :schema (keyword schema) :entity (keyword token) :partial [token [        :table]] :values (map parsed-value->indexed-value values) :or false}
                    [:RESOURCE [:entity                 [:partial-token token]] [:ors & values]]    {:type "condition"                          :entity (keyword token) :partial [token [:schema :table]] :values (map parsed-value->indexed-value values) :or true}
                    [:RESOURCE [:entity [:token schema] [:partial-token token]] [:ors & values]]    {:type "condition" :schema (keyword schema) :entity (keyword token) :partial [token [        :table]] :values (map parsed-value->indexed-value values) :or true}

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
          add-context        ;; [ [:type "condition" ... :context {:entity :users :alias "u"} ] ]
          )))

;; Build SQL from the AST


(defn table->sql
  "Table to sql"
  [connection table]
  (str "SELECT * FROM " (connection/quote connection (name table))))


(defn ast-join->sql
  "Convert a single join in the joins parts of the AST to an sql query
  TODO: The function assumes that the parameters are already escaped. This needs to be fixed."
  [table alias [t1 t2]]
  (format "JOIN %s AS %s ON (%s = %s)" table alias t1 t2) )

(defn ast-joins->sql
  "Convert joins part of the AST to an sql query"
  [joins]
  (->> joins
       (partition 3 3)
       (map (partial apply ast-join->sql))
       (s/join " ")
       )
  )

(defn ast->sql-and-params'
  "Create an sql query"
  [connection ast]
  (let [select-columns (s/join ", " (ast :select))
        [schema table alias]  (ast :from)
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
                  (->> [schema table]                                  ;; schema and table
                       (remove nil?)
                       (map name)
                       (map (partial connection/quote connection))
                       (s/join "."))
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
  [connection ast]
   (let [meta (:meta ast)]
     (cond meta [meta []]
           :else (ast->sql-and-params' connection ast))))

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
  [quoted-column _ alias]
  (format "%s.%s" alias quoted-column)
  )


(defn primary-key
  "Get the qualified primary key for the table. This is a naive function that
  assumes that the primary key is always Id."
  [connection op]
  (format "%s.%s" (op :alias) (connection/quote connection "id")))

;; -----------------
;; Operations to AST
;; -----------------

(defn alias->select-all
  "Get the columns for the last 'condition' operation or the select operation"
  [connection op]
  (let [entity (or (:entity op) (-> op :context :entity))
        alias (table-alias op)]
    ;; (map #(format "%s.%s" alias (connection/quote connection %)) (db/get-columns connection entity))
    [(format "%s.*" alias)]
    )
  )

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
  [ops]
  (->> ops
       (filter (operation-type? ["condition", "select"]))
       (partition 2 1)
       (filter (fn [[a b]] (and (operation-type? ["condition"] a)
                               (operation-type? ["select"] b))))
       (map (fn [pair] (let [a       (table-alias (first pair))
                             columns (:columns (second pair))]
                         (map (fn [c] (qualify c :with a)) columns)
                         )))
       (apply concat)
       )
  )


(defn operations->select-columns
  "Get the columns for the last 'condition' operation or the select operation"
  [connection operations]
  (let [specific-columns    (operations->select-specific operations)
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
                                                            (alias->select-all connection)
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
        ]

      columns-before-exclusion
      )
    )
  )

(defn relation->join
  [connection ta f-ta relations]
  (let [c (-> relations :via keys
                first  ;; Get the first column
                )
        join (-> (get-in relations [:via c])
                 first ;; Get the first relation for that column
                 )
        [schema table col _ f-schema f-table f-col] join
        col (connection/quote connection col)
        f-col (connection/quote connection f-col)
        ]
    [(qualify col :with ta) (qualify f-col :with f-ta)]
    ))

(defn operations->join
  [connection o1 o2]
  (let [md (connection/get-metadata connection)
        a1 (table-alias o1)
        a2 (table-alias o2)
        s2 (:schema o2)
        e1 (->> o1 :entity name) ;; TODO: rule#1
        e2 (->> o2 :entity name) ;; TODO: rule#1
        join (or (when-let [relation (get-in md [:db/references :table e1 :refers-to e2])]
                   (relation->join connection a1 a2 relation))
                 (when-let [relation (get-in md [:db/references :table e2 :refers-to e1])]
                   (relation->join connection a2 a1 relation)))]
    [(connection/quote connection s2 e2) a2 (or join ["1" "2 /* Not related!! */"])]))


(defn operations->joins
  "Get the joins from the operations"
  [c operations]
  (let [[op & ops] operations]
    (->>
     (cond (nil? ops) []
           :else (reduce (fn [acc [o1 o2]]
                           (concat acc (operations->join c o1 o2))
                           )
                         [] (partition 2 1 operations)))
     (apply vector)
     )
    )
  )

(defn filter->where-condition
  "Convert the filter part of an operation to a where sql"
  [connection entity qualify? [column [type value] op]]
   (let [col (cond qualify? (qualify (connection/quote connection column) :with (name entity)) :else column)
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
  [connection qualify? operation]
   (let [vs (:values operation)
         or (:or operation)]
     (cond (empty? vs) { :conditions "true" :params nil}
           :else       (->> vs
                            (map (partial filter->where-condition connection (:alias operation) qualify?))
                            ((fn [where-conditions] { :conditions (str "(" (->> where-conditions
                                                                                (map first)
                                                                                (s/join (cond or " OR " :else " AND "))) ")")
                                                     :params (->> where-conditions
                                                                  (map second)
                                                                  vec
                                                                  ) }))

                            ))))

(defn operations->where
  "Get the conditions from the operations"
  [connection qualify? ops]
   (let [
         wheres (map (partial operation->where connection qualify?) ops)
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
         ((fn [op]
            (cond (:count op) (format "LIMIT %s" (:count op)))
            ))
         ))
  )

(defn operations->order
  "Create the ORDER BY SQL statement"
  [c ops]
  (let [[condition-op order-op] (->> ops
                                     (filter (operation-type? ["condition", "order"]))
                                     (partition 2 1)
                                     (filter (fn [ops] (and (operation-type? ["condition"] (first ops))
                                                            (operation-type? ["order"] (second ops)))))
                                     (last))]

    (cond (and condition-op order-op) (let [direction (order-op :direction)
                                            column (order-op :column)
                                            a      (table-alias condition-op)]
                                        (format "ORDER BY %s.%s %s" a (connection/quote c column) (cond (= direction "ascending") "ASC" :else "DESC")))
          :else nil)))

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
  [c op]
  (let [values (op :values)
        alias  (table-alias op)
        ]
    {
     :values (->> values
                  (map first)
                  (map (fn [column]
                         (str (connection/quote c column) " = ?"))))
     :params (->> values
                  (map second)
                  vec)
     }))

(defn operations->set
  "Create the AST for the SET part"
  [connection operations]
  (let [ops (filter (operation-type? ["set"]) operations)]
    (cond (empty? ops) nil
          :else        (->> ops
                            last
                            (operation->set connection))))
  )

(defn operations->ast
  "operations to ast"
  [c ops]
  (let [
        columns       (operations->select-columns c ops)
        condition-ops (filter (operation-type? ["condition"]) ops)
        primary-op    (first condition-ops)
        table-group   (:schema primary-op)
        table         (:entity primary-op)
        delete        (operations->delete ops)
        set-values    (operations->set c ops)
        joins         (operations->joins c condition-ops)
        where         (operations->where c (cond delete nil :else true) condition-ops)
        order         (operations->order c ops)
        group         (operations->group ops)
        limit         (operations->limit ops)
        meta          (operations->meta ops)
        ]
    {
     :select columns
     :from [table-group table (table-alias primary-op)]
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
