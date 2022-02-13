(ns pine.db
  (:require [pine.config :as c]
            [pine.db.mysql :as mysql ]
            [pine.db.postgres :as postgres]
            [pine.db.protocol :as protocol]
            )
  (:import pine.db.mysql.MysqlConnection)
  (:import pine.db.postgres.PostgresConnection)
  )

(defn get-connections [] (c/config :connections))
(defn get-connection [id] (let [config ((get-connections) id)
                                type (config :dbtype)]
                            (cond (= type "mysql") (MysqlConnection. id config)
                                  (= type "postgres") (PostgresConnection. id config)
                                  :else (throw (Exception. (format "Db not supported: %s" type))))))

(def connection (->> :connection-id
                     c/config
                     get-connection
                     atom))

;; DB wrappers
(defn quote [x]
  (protocol/quote @connection x))

(defn quote-string [x]
  (protocol/quote-string @connection x))

(defn references [schema table]
  (protocol/references @connection schema table))

(defn relation
  "Get the column that has the relationship between the tables:
  (relation :caseFiles: :owns :documents:) => \"caseFileId\"
  (relation :documents: :owned-by :caseFile:) => \"caseFileId\"
  "
  [schema t1 relationship t2]
  (case relationship
    :owns     (t1 (protocol/references @connection schema t2))
    :owned-by (t2 (protocol/references @connection schema t1))
    :else     nil)
  )

(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  (protocol/get-columns @connection schema table-name))

;; Helpers

(defn $
  "Execute raw sql queries:
  ($ \"select id from users limit 10\")
  ($ first \"select id from users limit 10\")
  "
  ([fn query]
   (->> query
        (protocol/query @connection)
        fn))
  ([query]
   ($ identity query)))

(defn $!
  "Execute non select queries:
  ($ \"use tmp\")
  "
  ([fn query]
   (->> query
        (protocol/execute! @connection)
        fn))
  ([query]
   ($! identity query)))

;; ($ "show tables")
;; ($! "use tmp")
