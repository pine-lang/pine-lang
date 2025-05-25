(ns pine.parser
  "The parser is responsible for generating a parse tree from the bnf and
  normalize the output which is used for the input for generating the ast"
  (:require
   [clojure.core.match :refer [match]]
   [clojure.string :as s]
   [instaparse.core :as insta]
   [pine.data-types :as dt]))

(defmulti -normalize-op
  "Normalize the output of the parser. The first argument is the type of the
  operation"
  first)

;; ----
;; FROM
;; ----

(defmethod -normalize-op :TABLE [payload]
  (match payload

         ;; table
    [:TABLE [:symbol table]]                                        {:type :table, :value {:table table}}
    [:TABLE "has:" [:symbol table]]                                 {:type :table, :value {:table table :parent false}}
    [:TABLE "of:" [:symbol table]]                                  {:type :table, :value {:table table :parent true}}
    [:TABLE [:symbol table] "^"]                                    {:type :table, :value {:table table :parent true}}

    ;; schema.table
    [:TABLE [:symbol schema] [:symbol table]]                       {:type :table, :value {:table table :schema schema}}
    [:TABLE "has:" [:symbol schema] [:symbol table]]                {:type :table, :value {:table table :schema schema :parent false}}
    [:TABLE "of:" [:symbol schema] [:symbol table]]                 {:type :table, :value {:table table :schema schema :parent true}}
    [:TABLE [:symbol schema] [:symbol table] "^"]                   {:type :table, :value {:table table :schema schema :parent true}}

    ;; table aliaas
    [:TABLE [:symbol table] [:alias [:symbol a]]]                   {:type :table, :value {:table table :alias a}}

    ;; schema.table alias
    [:TABLE  [:symbol schema] [:symbol table] [:alias [:symbol a]]]        {:type :table, :value {:schema schema :table table :alias a}}
    [:TABLE  "has:" [:symbol schema] [:symbol table] [:alias [:symbol a]]] {:type :table, :value {:schema schema :table table :alias a :parent false}}
    [:TABLE  "of:" [:symbol schema] [:symbol table] [:alias [:symbol a]]]  {:type :table, :value {:schema schema :table table :alias a :parent true}}
    [:TABLE [:symbol schema] [:symbol table] "^" [:alias [:symbol a]]]     {:type :table, :value {:schema schema :table table :alias a :parent true}}

    ;; table .column
    [:TABLE [:symbol table] [:hint-column [:symbol column]]]              {:type :table, :value {:table table :join-column column}}
    [:TABLE "has:" [:symbol table] [:hint-column [:symbol column]]]       {:type :table, :value {:table table :join-column column :parent false}}
    [:TABLE "of:" [:symbol table] [:hint-column [:symbol column]]]        {:type :table, :value {:table table :join-column column :parent true}}
    [:TABLE [:symbol table] "^" [:hint-column [:symbol column]]]          {:type :table, :value {:table table :join-column column :parent true}}

    ;; schema.table .column
    [:TABLE [:symbol schema] [:symbol table] [:hint-column [:symbol column]]]         {:type :table, :value {:schema schema :table table :join-column column}}
    [:TABLE "has:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]]]  {:type :table, :value {:schema schema :table table :join-column column :parent false}}
    [:TABLE "of:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]]]   {:type :table, :value {:schema schema :table table :join-column column :parent true}}
    [:TABLE [:symbol schema] [:symbol table] "^" [:hint-column [:symbol column]]]     {:type :table, :value {:schema schema :table table :join-column column :parent true}}

    ;; schema.table .column alias
    [:TABLE [:symbol schema] [:symbol table] [:hint-column [:symbol column]] [:alias [:symbol a]]]         {:type :table, :value {:schema schema :table table :join-column column :alias a}}
    [:TABLE "has:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]] [:alias [:symbol a]]]  {:type :table, :value {:schema schema :table table :join-column column :alias a :parent false}}
    [:TABLE "of:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]] [:alias [:symbol a]]]   {:type :table, :value {:schema schema :table table :join-column column :alias a :parent true}}
    [:TABLE [:symbol schema] [:symbol table] "^" [:hint-column [:symbol column]] [:alias [:symbol a]]]     {:type :table, :value {:schema schema :table table :join-column column :alias a :parent true}}

    ;; Empty table
    [:TABLE]                                                        {:type :table, :value {:table ""}}

    :else
    (throw (ex-info "Unknown RESOURCE operation" {:_ payload}))))

;; -----------------------
;; SELECT / SELECT-PARTIAL
;; -----------------------

(defn- -normalize-column [column]
  (println "COLUMN" column)
  (match column
    [:aliased-column [:column [:symbol c]]]                                            {:column c}
    [:aliased-column [:column [:alias [:symbol a]] [:symbol c]]]                       {:alias a :column c}
    [:aliased-column [:column [:symbol c]] [:alias [:symbol ca]]]                      {:column c :column-alias ca}
    [:aliased-column [:column [:alias [:symbol a]] [:symbol c]] [:alias [:symbol ca]]] {:alias a :column c :column-alias ca}
    [:aliased-column [:alias [:symbol a]] [:star _star]]                                {:alias a :column "" :symbol "*"}
    :else                 (throw (ex-info "Unknown COLUMN operation" {:_ column}))))

(defn normalize-select [payload type]
  (match payload
    [:aliased-columns & columns] {:type type :value (mapv -normalize-column columns)}
    :else                (throw (ex-info (str "Unknown " (name type) " operation") {:_ payload}))))

(defmethod -normalize-op :SELECT [[_ payload]]
  (normalize-select payload :select))

(defmethod -normalize-op :SELECT-PARTIAL [[_ payload]]
  (if (empty? payload)
    {:type :select-partial, :value []}
    (normalize-select payload :select-partial)))

;; ------
;; ORDER
;; ------

(defn- -normalize-order-col [column]
  (match column
    [:order-column [:column [:symbol c]]]   {:column c :direction "DESC"}
    [:order-column [:column [:symbol c]] d] {:column c :direction (s/upper-case d)}
    :else                                   (throw (ex-info "Unknown ORDER operation" {:_ column}))))

(defn -normalize-order [payload type]
  (match payload
    [:order-columns & columns] {:type type :value (mapv -normalize-order-col columns)}
    :else                (throw (ex-info (str "Unknown " (name type) " operation") {:_ payload}))))

(defmethod -normalize-op :ORDER [[_ payload]]
  (-normalize-order payload :order))

(defmethod -normalize-op :ORDER-PARTIAL [[_ payload]]
  (if (empty? payload)
    {:type :order-partial, :value []}
    (-normalize-order payload :order-partial)))

;; -----
;; WHERE
;; -----

(defn- parse-characters [characters] {:type :string :value (apply str characters)})
(defn- parse-strings [[_ & characters]] (parse-characters characters))

(defn- parse-condition [condition]
  (match condition
    ;; Equals operations
    [:condition [:column [:symbol column]]                      [:equals] [:number value]]                                    {:type :where :value [(dt/column column) "=" (dt/number value)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:equals] [:number value]]                                    {:type :where :value [(dt/column a column) "=" (dt/number value)]}
    [:condition [:column [:symbol column]]                      [:equals] [:null]]                                            {:type :where :value [(dt/column column) "IS" (dt/symbol "NULL")]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:equals] [:null]]                                            {:type :where :value [(dt/column a column) "IS" (dt/symbol "NULL")]}
    [:condition [:column [:symbol column]]                      [:equals] [:column [:symbol c]]]                              {:type :where :value [(dt/column column) "=" (dt/column c)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:equals] [:column [:symbol c]]]                              {:type :where :value [(dt/column a column) "=" (dt/column c)]}
    [:condition [:column [:symbol column]]                      [:equals] [:column [:alias [:symbol a]] [:symbol c]]]         {:type :where :value [(dt/column column) "=" (dt/column a c)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:equals] [:column [:alias [:symbol a2]] [:symbol c]]]        {:type :where :value [(dt/column a column) "=" (dt/column a2 c)]}
    [:condition [:column [:symbol column]]                      [:equals] [:string & characters]]                             {:type :where :value [(dt/column column) "=" (parse-characters characters)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:equals] [:string & characters]]                             {:type :where :value [(dt/column a column) "=" (parse-characters characters)]}
    [:condition [:column [:symbol column]]                      [:equals] [:boolean b]]                                       {:type :where :value [(dt/column column) "=" (dt/symbol b)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:equals] [:boolean b]]                                       {:type :where :value [(dt/column a column) "=" (dt/symbol b)]}

    ;; Not equals operations
    [:condition [:column [:symbol column]]                      [:does-not-equal] [:number value]]                             {:type :where :value [(dt/column column) "!=" (dt/number value)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:does-not-equal] [:number value]]                             {:type :where :value [(dt/column a column) "!=" (dt/number value)]}
    [:condition [:column [:symbol column]]                      [:does-not-equal] [:null]]                                     {:type :where :value [(dt/column column) "IS NOT" (dt/symbol "NULL")]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:does-not-equal] [:null]]                                     {:type :where :value [(dt/column a column) "IS NOT" (dt/symbol "NULL")]}
    [:condition [:column [:symbol column]]                      [:does-not-equal] [:column [:symbol c]]]                       {:type :where :value [(dt/column column) "!=" (dt/column c)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:does-not-equal] [:column [:symbol c]]]                       {:type :where :value [(dt/column a column) "!=" (dt/column c)]}
    [:condition [:column [:symbol column]]                      [:does-not-equal] [:column [:alias [:symbol a]] [:symbol c]]]  {:type :where :value [(dt/column column) "!=" (dt/column a c)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:does-not-equal] [:column [:alias [:symbol a2]] [:symbol c]]] {:type :where :value [(dt/column a column) "!=" (dt/column a2 c)]}
    [:condition [:column [:symbol column]]                      [:does-not-equal] [:string & characters]]                      {:type :where :value [(dt/column column) "!=" (parse-characters characters)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:does-not-equal] [:string & characters]]                      {:type :where :value [(dt/column a column) "!=" (parse-characters characters)]}
    [:condition [:column [:symbol column]]                      [:does-not-equal] [:boolean b]]                                {:type :where :value [(dt/column column) "!=" (dt/symbol b)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:does-not-equal] [:boolean b]]                                {:type :where :value [(dt/column a column) "!=" (dt/symbol b)]}

    ;; IS NULL operations
    [:condition [:column [:symbol column]]                      [:is] [:null]]                                                {:type :where :value [(dt/column column) "IS" (dt/symbol "NULL")]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:is] [:null]]                                                {:type :where :value [(dt/column a column) "IS" (dt/symbol "NULL")]}
    [:condition [:column [:symbol column]]                      [:is-not] [:null]]                                            {:type :where :value [(dt/column column) "IS NOT" (dt/symbol "NULL")]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:is-not] [:null]]                                            {:type :where :value [(dt/column a column) "IS NOT" (dt/symbol "NULL")]}

    ;; LIKE operations
    [:condition [:column [:symbol column]]                      [:like] [:string & characters]]                               {:type :where :value [(dt/column column) "LIKE" (parse-characters characters)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:like] [:string & characters]]                               {:type :where :value [(dt/column a column) "LIKE" (parse-characters characters)]}

    ;; IN operations
    [:condition [:column [:symbol column]]                      [:in] & strings]                                              {:type :where :value [(dt/column column) "IN" (map parse-strings strings)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:in] & strings]                                              {:type :where :value [(dt/column a column) "IN" (map parse-strings strings)]}
    [:condition [:column [:symbol column]]                      [:not-in] & strings]                                          {:type :where :value [(dt/column column) "NOT IN" (map parse-strings strings)]}
    [:condition [:column [:alias [:symbol a]] [:symbol column]] [:not-in] & strings]                                          {:type :where :value [(dt/column a column) "NOT IN" (map parse-strings strings)]}

    :else                (throw (ex-info "Unknown condition in WHERE operation"      {:_ condition}))))

(defmethod -normalize-op :WHERE [[_ payload]]
  (match payload
    [:conditions & conditions] (first (map parse-condition conditions))
    :else                (throw (ex-info "Unknown WHERE operation"      {:_ payload}))))

;; -----
;; LIMIT
;; -----

(defmethod -normalize-op :LIMIT [[_ [_ number]]]
  {:type :limit
   :value (Integer/parseInt number)})

;; -----
;; FROM
;; -----

(defmethod -normalize-op :FROM [[_ payload]]
  (match payload
    [:alias [:symbol c]] {:type :from :value {:alias c}}
    :else (throw (ex-info "Unknown FROM operation" {:_ payload}))))

;; -----
;; COUNT
;; -----

(defmethod -normalize-op :COUNT [[_ [_ _payload]]]
  {:type :count :value {:column "*"}})

;; -----
;; GROUP
;; -----

(defmethod -normalize-op :GROUP [[_ payload]]
  (match payload
    [:group-args [:aliased-columns & columns] [:aggregate-functions [:aggregate-function function-name]]]
    {:type :group
     :value {:columns (mapv -normalize-column columns)
             :functions [function-name]}}
    :else (throw (ex-info "Unknown GROUP operation" {:_ payload}))))

;; -----
;; DELETE
;; -----

(defmethod -normalize-op :DELETE-ACTION [[_ [_ payload]]]
  (match payload
    [:symbol c] {:type :delete-action :value {:column c}}
    :else (throw (ex-info "Unknown DELETE operation" {:_ payload}))))

;; -----
;; NO-OP
;; -----

(defmethod -normalize-op :DELETE [_]
  {:type :delete :value nil})

;; -----

(def ^:private parser
  (let [dir (System/getProperty "user.dir")
        file (format "%s/src/pine/pine.bnf" dir)
        grammar (slurp file)]
    (insta/parser grammar)))

(defn- normalize-ops [[_ & ops]]
  (mapv (fn [[_ op]] (-normalize-op op)) ops))

(defn parse
  "Parse an expression and return the normalized operations or failure as a string"
  [expression]
  (let [result (parser expression)
        failure? (insta/failure? result)]
    (if failure?
      (let [failure (insta/get-failure result)
            error (with-out-str (println (insta/get-failure result)))]
        {:error error :failure failure})
      {:result (normalize-ops result)})))

(defn parse-or-fail [expression]
  (-> expression parser normalize-ops))
