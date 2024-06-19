(ns pine.db.main
  (:require [pine.db.postgres :as postgres]
            [pine.db.config :as config]))

;; Application state
(def connection-id "Currently selected connection" (atom :test))
(def references "References indexed by the connection id" (atom {}))

(defn init-references
  "Get the references for a given key"
  [id]
  (or
   (@references id)
   (do
     (swap! references assoc id (postgres/get-indexed-references id))
     (@references id))))

