(ns pine.db.main
  (:require [pine.db.postgres :as postgres]))

;; TODO: move to a config file
(def config {:default {;; :host "host.docker.internal" ;; for docker
                       :host "localhost"
                       :dbtype "?"
                       :dbname "?"
                       :user "?"
                       :password "?"
                       :schema nil}})

(def references (atom {}))

(defn init-references
  "Get the references for a given key"
  [id]
  (or
   (@references id)
   (do
     (swap! references assoc id (postgres/get-indexed-references (config id)))
     (@references id))))

