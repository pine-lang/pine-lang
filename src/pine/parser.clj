(ns pine.parser
  "The parser is responsible for generating a parse tree from the bnf and
  normalize the output which is used for the input for generating the ast"
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
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

;; ------
;; SELECT
;; ------

(defn- -normalize-column [column]
  (match column
    [:aliased-column [:column [:symbol c]]]                                     {:column c}
    [:aliased-column [:column [:alias [:symbol a]] [:symbol c]]]                {:alias a :column c}
    [:aliased-column [:column [:symbol c]] [:alias [:symbol ca]]]             {:column c :column-alias ca}
    [:aliased-column [:column [:alias [:symbol a]] [:symbol c]] [:alias [:symbol ca]]] {:alias a :column c :column-alias ca}

    :else                 (throw (ex-info "Unknown COLUMN operation" {:_ column}))))

(defmethod -normalize-op :SELECT [[_ payload]]
  (match payload
    [:aliased-columns & columns] {:type :select :value (mapv -normalize-column columns)}
    :else                (throw (ex-info "Unknown SELECT operation" {:_ payload}))))

;; -------------
;; SELECT-PARTIAL
;; -------------

(defmethod -normalize-op :SELECT-PARTIAL [_]
  {:type :select-partial :value []})

;; ------
;; ORDER
;; ------

(defn- -normalize-order [column]
  (match column
    [:order-column [:column [:symbol c]]]   {:column c :direction "DESC"}
    [:order-column [:column [:symbol c]] d] {:column c :direction (clojure.string/upper-case d)}
    :else                                   (throw (ex-info "Unknown ORDER operation" {:_ column}))))

(defmethod -normalize-op :ORDER [[_ payload]]
  (match payload
    [:order-columns & columns] {:type :order :value (mapv -normalize-order columns)}
    :else                (throw (ex-info "Unknown SELECT operation" {:_ payload}))))

;; -----
;; WHERE
;; -----

(defn- parse-characters [characters] {:type :string :value (apply str characters)})
(defn- parse-strings [[_ & characters]] (parse-characters characters))

(defn- parse-condition [condition]
  (match condition
    [:condition [:symbol column] [:equals] [:number value]]                                    {:type :where :value [column "=" (dt/number value)]}
    [:condition [:symbol column] [:equals] [:null]]                                            {:type :where :value [column "IS" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:equals] [:column [:symbol c]]]                              {:type :where :value [column "=" (dt/column c)]}
    [:condition [:symbol column] [:equals] [:column [:alias [:symbol a]] [:symbol c]]]         {:type :where :value [column "=" (dt/column a c)]}
    [:condition [:symbol column] [:equals] [:string & characters]]                             {:type :where :value [column "=" (parse-characters characters)]}
    [:condition [:symbol column] [:does-not-equal] [:number value]]                            {:type :where :value [column "!=" (dt/number value)]}
    [:condition [:symbol column] [:does-not-equal] [:null]]                                    {:type :where :value [column "IS NOT" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:does-not-equal] [:column [:symbol c]]]                      {:type :where :value [column "!=" (dt/column c)]}
    [:condition [:symbol column] [:does-not-equal] [:column [:alias [:symbol a]] [:symbol c]]] {:type :where :value [column "!=" (dt/column a c)]}
    [:condition [:symbol column] [:does-not-equal] [:string & characters]]                     {:type :where :value [column "!=" (parse-characters characters)]}
    [:condition [:symbol column] [:is] [:null]]                                                {:type :where :value [column "IS" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:is-not] [:null]]                                            {:type :where :value [column "IS NOT" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:like] [:string & characters]]                               {:type :where :value [column "LIKE" (parse-characters characters)]}
    [:condition [:symbol column] [:in] & strings]                                              {:type :where :value [column "IN" (map parse-strings strings)]}
    [:condition [:symbol column] [:not-in] & strings]                                          {:type :where :value [column "NOT IN" (map parse-strings strings)]}
    [:condition [:symbol column] [:equals] [:boolean b]]                                       {:type :where :value [column "=" (dt/symbol b)]}
    [:condition [:symbol column] [:does-not-equal] [:boolean b]]                               {:type :where :value [column "!=" (dt/symbol b)]}
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

(defmethod -normalize-op :COUNT [[_ [_ payload]]]
  {:type :count :value {:column "*"}})

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

(defn parse [expression]
  "Parse an expression and return the normalized operations"
  (let [result (parser expression)
        failure? (insta/failure? result)]
    (if failure?
      result
      (normalize-ops result))))

(defn parse-or-fail [expression]
  (-> expression parser normalize-ops))

(defn parse [expression]
  "Parse an expression and return the normalized operations or failure as a string"
  (let [result (parser expression)
        failure? (insta/failure? result)]
    (if failure?
      (let [failure (insta/get-failure result)
            error (with-out-str (println (insta/get-failure result)))]
        {:error error :failure failure})
      {:result (normalize-ops result)})))
