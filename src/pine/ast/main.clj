(ns pine.ast.main
  (:require [pine.parser :as parser]
            [pine.ast.table :as table]
            [pine.ast.limit :as limit]
            [pine.db.main :as db]
            [pine.ast.where :as where]))

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
            :connection-id nil
            :references {}})

(defn handle [state {:keys [type value]}]
  (case type
    :table (table/handle state value)
    :limit (limit/handle state value)
    :where (where/handle state value)
    (update state :errors conj [type "Unknown operation type in parse tree"])))

(defn generate
  ([parse-tree]
   (generate parse-tree @db/connection-id))
  ([parse-tree connection-id]
   (let [state (-> state
                   (assoc :references (db/init-references connection-id))
                   (assoc :connection-id connection-id))]
     (reduce handle state parse-tree))))
