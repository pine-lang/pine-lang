(ns pine.ast.main
  (:require [pine.parser :as parser]
            [pine.ast.table :as table]
            [pine.ast.limit :as limit]
            [pine.db.main :as db]))

(def init-state {;; ast
                 :tables        []       ;; e.g. [{ :table "user" :schema "public" :alias "u" }] ;; schema is nilable
                 :table-count   0
                 :columns       []       ;; e.g. [{ :alias "u" :column "name" }]
                 :limit         nil      ;; number ;; nilable
                 :aliases       {}       ;; e.g. [{ :schema "public" :table "user" }] ;; schema is nilable
                 :joins         {}       ;; I don't know - this can be anything at the moment

            ;; connection
                 :references {}})

(defn handle [state {:keys [type value]}]
  (case type
    :table (table/handle state value)
    :limit (limit/handle state value)
    (update state :errors conj [type "Unknown type"])))

(defn generate [parse-tree]
  (let [state (assoc init-state :references (db/init-references @db/connection-id))]
    (reduce handle state parse-tree)))
