(ns pine.db.connections)

(defn get-env [var default]
  (or (System/getenv var) default))

;; handle port
(def connections "Database connections"
  (atom {:default {:host (get-env "DB_HOST" nil)
                   :dbtype (get-env "DB_TYPE" "postgres")
                   :port (get-env "DB_PORT" 5432)
                   :dbname (get-env "DB_NAME" nil)
                   :user (get-env "DB_USER" nil)
                   :password (get-env "DB_PASSWORD" nil)
                   :schema (get-env "DB_SCHEMA" nil)}}))

(defn get-connection [id]
  (let [connection (@connections id)]
    (if connection
      connection
      (throw (ex-info "Connection not found" {:id id})))))

;; connection is of the format
(defn add-connection [connection]
  (let [id (connection :host)]
    (swap! connections assoc id connection)
    id))

(defn get-connection-name [id]
  (-> id get-connection :host))
