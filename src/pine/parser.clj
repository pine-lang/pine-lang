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

;; ------
;; ORDER
;; ------

(defn- -normalize-order[column]
  (match column
         [:column [:symbol c]] {:column c :direction "DESC"} ;; TODO: direction is hardcoded
         :else                 (throw (ex-info "Unknown ORDER operation" {:_ column}))))

(defmethod -normalize-op :ORDER [[_ payload]]
  (match payload
    [:columns & columns] {:type :order :value (mapv -normalize-order columns)}
    :else                (throw (ex-info "Unknown SELECT operation" {:_ payload}))))

;; -----
;; WHERE
;; -----

(defn- parse-characters [[_ & characters]] {:type :string :value (apply str characters)})

(defmethod -normalize-op :WHERE [[_ payload]]
  (match payload
    [:condition [:symbol column] [:equals _] [:number value]]        {:type :where :value [column "=" (dt/number value)]}
    [:condition [:symbol column] [:is _] [:null _]]                  {:type :where :value [column "IS" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:is _] [:not _] [:null _]]         {:type :where :value [column "IS NOT" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:equals _] [:null _]]              {:type :where :value [column "IS" (dt/symbol "NULL")]}
    [:condition [:symbol column] [:equals _] string]                 {:type :where :value [column "=" (parse-characters string)]}
    [:condition [:symbol column] [:like _] string]                   {:type :where :value [column "LIKE" (parse-characters string)]}
    [:condition [:symbol column] [:in _]  & strings]                 {:type :where :value [column "IN" (map parse-characters strings)]}
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
;; DELETE
;; -----

(defmethod -normalize-op :DELETE [[_ [_ payload]]]
  (match payload
    [:symbol c] {:type :delete :value {:column c}}
    :else (throw (ex-info "Unknown DELETE operation" {:_ payload}))))

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
