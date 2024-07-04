(ns pine.util
  (:require [pine.db.config :as config]))

(defn get-connection [id]
  (let [connection (config/connections id)]
    (if connection
      connection
      (throw (ex-info "Connection not found" {:id id})))))

(defn get-connection-name [id]
  (-> id get-connection :host))
