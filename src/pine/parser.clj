(ns pine.parser
  "The parser is responsible for generating a parse tree from the bnf and
  normalize the output which is used for the input for generating the ast"
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]))

(defmulti -normalize-op
  "Normalize the output of the parser. The first argument is the type of the
  operation"
  first)

;; -----
;; TABLE
;; -----

(defmethod -normalize-op :TABLE [payload]
  (match payload
    [:TABLE [:qualified-symbol [:partial-symbol [:symbol table]]]]                     {:type :table, :value {:table table}}
    [:TABLE [:qualified-symbol [:partial-symbol]]]                                     {:type :table, :value {:table ""}}
    [:TABLE [:qualified-symbol [:symbol schema] [:partial-symbol [:symbol table]]]]    {:type :table, :value {:table table :schema schema}}
    [:TABLE [:qualified-symbol [:symbol table] [:alias [:symbol a]]]]                  {:type :table, :value {:table table :alias a}}
    [:TABLE [:qualified-symbol [:symbol schema] [:symbol table] [:alias [:symbol a]]]] {:type :table, :value {:schema schema :table table :alias a}}
    [:TABLE [:qualified-symbol [:partial-symbol [:symbol table]]]
     [:join-column [:symbol column]]]                                                  {:type :table, :value {:table table :join-column column}}
    :else
    (throw (ex-info "Unknown RESOURCE operation" {:_ payload}))))

;; ------
;; SELECT
;; ------

(defn- -normalize-column [column]
  (match column
    [:column [:qualified-symbol [:partial-symbol [:symbol c]]]] {:column c}
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
    [:condition [:symbol column] [:operator op] [:number value]] {:type :where :value [column op value]}
    [:condition
     [:symbol column]
     [:operator op]
     [:string & characters]] {:type :where :value [column op (apply str (map second characters))]}
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
    [:qualified-symbol [:partial-symbol [:symbol c]]] {:type :delete
                                                       :value {:column c}}
    :else (throw (ex-info "Unknown DELETE operation" {:_ payload}))))

;; -----

(def ^:private parse-expression (let [dir (System/getProperty "user.dir")
                                      file (format "%s/src/pine/pine.bnf" dir)
                                      grammar (slurp file)]
                                  (insta/parser grammar)))

(defn- normalize-ops [[_ & ops]]
  (mapv (fn [[_ op]] (-normalize-op op)) ops))

(defn parse [expression]
  "Parse an expression and return the normalized operations"
  (-> expression
      parse-expression
      normalize-ops))

