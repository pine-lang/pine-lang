(ns pine.ast.main
  (:require [pine.parser :as parser]
            [pine.ast.table :as table]
            [pine.ast.limit :as limit]
            [pine.db.main :as db]
            [pine.ast.where :as where]
            [pine.hints :as hints]))

(def state {;; pre
            ;; - connection
            :connection-id nil
            :references {}

            ;; ast
            :tables         []           ;; e.g. [{ :table "user" :schema "public" :alias "u" }] ;; schema is nilable
            :columns        []           ;; e.g. [{ :alias "u" :column "name" }]
            :limit          nil          ;; number ;; nilable
            :aliases        {}           ;; e.g. [{ :schema "public" :table "user" }] ;; schema is nilable
            :joins          {}           ;; This can be a map or a vector - will decide later

            ;; state
            :operation      {:type  nil
                             :value nil} ;; [ ] 1. For post-handle. e.g. set hints if operation is table.
                                         ;; [ ] 2. For backwards compat with version < 0.5.
                                         ;;        If op is :table, then the context  in the api handler has one less table
            :context        nil
            :table-count    0
            :pending-count  0

            ;; post
            ;; - hints
            :hints          {:table []}})

(defn pre-handle [state connection-id ops-count]
  (-> state
      (assoc :references (db/init-references connection-id))
      (assoc :connection-id connection-id)
      (assoc :pending-count ops-count)))

(defn handle-op [state {:keys [type value]}]
  (case type
    :table (table/handle state value)
    :limit (limit/handle state value)
    :where (where/handle state value)
    (update state :errors conj [type "Unknown operation type in parse tree"])))

(defn handle-ops [state ops]
  (reduce (fn [s o] (-> s
                        (handle-op o)
                        (update :pending-count dec)
                        (assoc :operation o))) state ops))

(defn post-handle [state]
  (-> state
      (dissoc :references)))

(defn generate
  ([parse-tree]
   (generate parse-tree @db/connection-id))
  ([parse-tree connection-id]
   (-> state
       (pre-handle connection-id (count parse-tree))
       (handle-ops parse-tree)
       post-handle)))
