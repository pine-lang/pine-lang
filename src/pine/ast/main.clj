(ns pine.ast.main
  (:require [pine.parser :as parser]
            [pine.ast.table :as table]
            [pine.ast.limit :as limit]
            [pine.db.main :as db]))

(def state {;; ast
            :tables        []       ;; e.g. [{ :table "user" :schema "public" :alias "u" }] ;; schema is nilable
            :table-count   0
            :columns       []       ;; e.g. [{ :alias "u" :column "name" }]
            :limit         nil      ;; number ;; nilable
            :aliases       {}       ;; e.g. [{ :schema "public" :table "user" }] ;; schema is nilable
            :joins         {}       ;; I don't know - this can be anything at the moment
            ;; connection
            :connection-id :default ;; This should match the id of the connection in the config
            })

(defn handle [state {:keys [type value]}]
  (let [_ (db/init-references (state :connection-id))] ;; TODO: use the updated state with the connection info
    (case type
      :table (table/handle state value)
      :limit (limit/handle state value)
      (update state :errors conj [type "Unknown type"]))))

(defn generate [parse-tree]
  (reduce handle state parse-tree))
