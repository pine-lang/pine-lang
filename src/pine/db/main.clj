(ns pine.db.main
  (:require [pine.db.postgres :as postgres]))

;; Application state
(def connection-id "Currently selected connection" (atom nil))
(def references "References indexed by the connection id" (atom {}))

(def memoize-references true)
;; (def memoize-references false)

(defn init-references
  "Get the references for a given key"
  [id]
  (or
   (and memoize-references
        (@references id))
   (do
     (swap! references assoc id (postgres/get-indexed-references id))
     (@references id))))

(defn run-query [id query]
  (postgres/run-query id query))
