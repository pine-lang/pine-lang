(ns pine.db.config)

(defn get-env [var default]
  (or (System/getenv var) default))

(def connections {
                  :connection-id :default
                  ;; :dev {:host "localhost" ;; use for docker: "host.docker.internal"
                  ;;       :dbtype "postgres"
                  ;;       :dbname "?"
                  ;;       :user "?"
                  ;;       :password "?"
                  ;;       :schema nil}
                  :default {:host (get-env "DB_HOST" nil)
                           :dbtype (get-env "DB_TYPE" "postgres")
                           :dbname (get-env "DB_NAME" nil)
                           :user (get-env "DB_USER" nil)
                           :password (get-env "DB_PASSWORD" nil)
                           :schema (get-env "DB_SCHEMA" nil)}})
