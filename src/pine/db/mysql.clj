(ns pine.db.mysql
  (:require [clojure.java.jdbc :as jdbc]
            [pine.db.util :as u]
            [pine.db.connection :refer [Connection]]

            [pine.db.connection :as connection])
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  )

;; Connection Pooling
(defn- pool
  [spec]
  (let [cpds (doto (ComboPooledDataSource.)
               (.setDriverClass (:classname spec))
               (.setJdbcUrl (str "jdbc:" (:dbtype spec) ":" (:subname spec)))
               (.setUser (:user spec))
               (.setPassword (:password spec))
               ;; expire excess connections after 30 minutes of inactivity:
               (.setMaxIdleTimeExcessConnections (* 30 60))
               ;; expire connections after 3 hours of inactivity:
               (.setMaxIdleTime (* 3 60 60))
               )
        ]
    {:datasource cpds}))


(deftype Mysql [id config]
  Connection
  (get-connection-id [this]
    id)

  (get-metadata
    [this]
    (throw (Exception. "Not implemented yet")))

  (quote [this x]
    (format "`%s`" (name x)))
  (quote [this x y]
    (cond (not (nil? x)) (format "%s.%s" (connection/quote this x) (connection/quote this y))
          :else (connection/quote this y))
    )

  (quote-string [this x]
    (format "\"%s\"" x))

  (query [this statement]
    (jdbc/query config statement))

  (execute! [this statement]
    (jdbc/execute! config statement))

  (get-config [this]
    config)
  )
