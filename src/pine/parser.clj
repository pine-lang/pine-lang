(ns pine.parser
  "The parser is responsible for generating a parse tree from the bnf and
  normalize the output which is used for the input for generating the ast"
  (:require [instaparse.core :as insta]))

(def ^:private parse (let [dir (System/getProperty "user.dir")
                           file (format "%s/src/pine/pine.bnf" dir)
                           grammar (slurp file)]
                       (insta/parser grammar)))

(defmulti -normalize-op
  "Normalize the output of the parser. The first argument is the type of the
  operation"
  first)

(defmethod -normalize-op :RESOURCE [[_ [_ entity]]]
  {:type :resource
   :entity (second entity)})

(defmethod -normalize-op :SELECT [[_ [_ [_ & columns]]]]
  {:type :select
   :columns (map (fn [[_ [_ col]]] col) columns)})

(defmethod -normalize-op :LIMIT [[_ [_ number]]]
  {:type :limit
   :limit (Integer/parseInt number)})

(defn- normalize-ops [[_ & ops]]
  (mapv (fn [[_ op]] (-normalize-op op)) ops))

(defn parse-expression [expression]
  "Parse an expression and return the normalized operations"
  (let [parsed (parse expression)]
    (normalize-ops parsed))
  (->> expression
       parse
       normalize-ops))
