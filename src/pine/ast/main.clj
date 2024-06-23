(ns pine.ast.main
  (:require [pine.parser :as parser]
            [pine.ast.table :as table]
            [pine.ast.limit :as limit]
            [pine.db.main :as db]))

(def state {;; ast
            :tables        []       ;; e.g. [{ :table "user" :schema "public" :alias "u" }] ;; schema is nilable
            :columns       []       ;; e.g. [{ :alias "u" :column "name" }]
            :limit         nil      ;; number ;; nilable
            :aliases       {}       ;; e.g. [{ :schema "public" :table "user" }] ;; schema is nilable
            :joins         {}       ;; This can be a map or a vector - will decide later
            :suggestions   {}
            :context       nil
            :table-count   0

            ;; connection
            :references {}})

(defn handle [state {:keys [type value]}]
  (case type
    :table (table/handle state value)
    :limit (limit/handle state value)
    (update state :errors conj [type "Unknown type"])))

(defn generate
  ([parse-tree]
   (generate parse-tree @db/connection-id))
  ([parse-tree connection-id]
   (let [state (assoc state :references (db/init-references connection-id))]
     (reduce handle state parse-tree))))
