(ns pine.parser
  "The parser is responsible for generating a parse tree from the bnf and
  normalize the output which is used for the input for generating the ast"
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]))

(defmulti -normalize-op
  "Normalize the output of the parser. The first argument is the type of the
  operation"
  first)

;; ----
;; FROM
;; ----

(defmethod -normalize-op :FROM [payload]
  (match payload

         ;; table
    [:FROM [:symbol table]]                                        {:type :table, :value {:table table}}
    [:FROM "has:" [:symbol table]]                                 {:type :table, :value {:table table :parent false}}
    [:FROM "of:" [:symbol table]]                                  {:type :table, :value {:table table :parent true}}
    [:FROM [:symbol table] "^"]                                    {:type :table, :value {:table table :parent true}}

    ;; schema.table
    [:FROM [:symbol schema] [:symbol table]]                       {:type :table, :value {:table table :schema schema}}
    [:FROM "has:" [:symbol schema] [:symbol table]]                {:type :table, :value {:table table :schema schema :parent false}}
    [:FROM "of:" [:symbol schema] [:symbol table]]                 {:type :table, :value {:table table :schema schema :parent true}}
    [:FROM [:symbol schema] [:symbol table] "^"]                   {:type :table, :value {:table table :schema schema :parent true}}

    ;; table aliaas
    [:FROM [:symbol table] [:alias [:symbol a]]]                   {:type :table, :value {:table table :alias a}}

    ;; schema.table alias
    [:FROM  [:symbol schema] [:symbol table] [:alias [:symbol a]]]        {:type :table, :value {:schema schema :table table :alias a}}
    [:FROM  "has:" [:symbol schema] [:symbol table] [:alias [:symbol a]]] {:type :table, :value {:schema schema :table table :alias a :parent false}}
    [:FROM  "of:" [:symbol schema] [:symbol table] [:alias [:symbol a]]]  {:type :table, :value {:schema schema :table table :alias a :parent true}}
    [:FROM [:symbol schema] [:symbol table] "^" [:alias [:symbol a]]]     {:type :table, :value {:schema schema :table table :alias a :parent true}}

    ;; table .column
    [:FROM [:symbol table] [:hint-column [:symbol column]]]              {:type :table, :value {:table table :join-column column}}
    [:FROM "has:" [:symbol table] [:hint-column [:symbol column]]]       {:type :table, :value {:table table :join-column column :parent false}}
    [:FROM "of:" [:symbol table] [:hint-column [:symbol column]]]        {:type :table, :value {:table table :join-column column :parent true}}
    [:FROM [:symbol table] "^" [:hint-column [:symbol column]]]          {:type :table, :value {:table table :join-column column :parent true}}

    ;; schema.table .column
    [:FROM [:symbol schema] [:symbol table] [:hint-column [:symbol column]]]         {:type :table, :value {:schema schema :table table :join-column column}}
    [:FROM "has:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]]]  {:type :table, :value {:schema schema :table table :join-column column :parent false}}
    [:FROM "of:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]]]   {:type :table, :value {:schema schema :table table :join-column column :parent true}}
    [:FROM [:symbol schema] [:symbol table] "^" [:hint-column [:symbol column]]]     {:type :table, :value {:schema schema :table table :join-column column :parent true}}

    ;; schema.table .column alias
    [:FROM [:symbol schema] [:symbol table] [:hint-column [:symbol column]] [:alias [:symbol a]]]         {:type :table, :value {:schema schema :table table :join-column column :alias a}}
    [:FROM "has:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]] [:alias [:symbol a]]]  {:type :table, :value {:schema schema :table table :join-column column :alias a :parent false}}
    [:FROM "of:" [:symbol schema] [:symbol table] [:hint-column [:symbol column]] [:alias [:symbol a]]]   {:type :table, :value {:schema schema :table table :join-column column :alias a :parent true}}
    [:FROM [:symbol schema] [:symbol table] "^" [:hint-column [:symbol column]] [:alias [:symbol a]]]     {:type :table, :value {:schema schema :table table :join-column column :alias a :parent true}}

    ;; Empty table
    [:FROM]                                                        {:type :table, :value {:table ""}}

    :else
    (throw (ex-info "Unknown RESOURCE operation" {:_ payload}))))

;; ------
;; SELECT
;; ------

(defn- -normalize-column [column]
  (match column
    [:column [:symbol c]]                                   {:column c}
    [:column [:symbol a] [:symbol c]]                       {:alias a :column c}
    [:column [:symbol c]]                                   {:column c}
    [:column [:symbol c] [:alias [:symbol ca]]]             {:column c :column-alias ca}
    [:column [:symbol a] [:symbol c] [:alias [:symbol ca]]] {:alias a :column c :column-alias ca}

    :else                 (throw (ex-info "Unknown COLUMN operation" {:_ column}))))

(defmethod -normalize-op :SELECT [[_ payload]]
  (match payload
    [:columns & columns] {:type :select :value (mapv -normalize-column columns)}
    :else                (throw (ex-info "Unknown SELECT operation" {:_ payload}))))

;; -----
;; WHERE
;; -----

(defmethod -normalize-op :WHERE [[_ payload]]
  (match payload
    [:condition [:symbol column] [:operator op] [:number value]]                {:type :where :value [column op value]}
    [:condition [:symbol column] [:operator op] [:string & characters]]         {:type :where :value [column op (apply str characters)]}
    [:condition [:symbol column] op  & strings]                         {:type :where :value [column op (map #(apply str (rest %1)) strings)]}
    :else                (throw (ex-info "Unknown WHERE operation" {:_ payload}))))

;; -----
;; LIMIT
;; -----

(defmethod -normalize-op :LIMIT [[_ [_ number]]]
  {:type :limit
   :value (Integer/parseInt number)})

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
