(ns pine.db
  (:require [pine.config :as c]
            [pine.db.mysql :as mysql ]
            [pine.db.postgres :as postgres]
            [pine.db.protocol :as protocol]
            )
  (:import pine.db.mysql.MysqlAdapter)
  (:import pine.db.postgres.PostgresAdapter)
  )

(defn get-connection [id] (let [config ((c/config :connections) id)
                                type (config :dbtype)]
                            (cond (= type "mysql") (MysqlAdapter. config)
                                  (= type "postgres") (PostgresAdapter. config)
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

;; ;; TODO: move to adapter
;; (defn get-schema' []
;;   (protocol/get-schema adapter))
;; (def get-schema (memoize get-schema'))

;; (protocol/query adapter "show tables;") ;; mysql
;; (protocol/query adapter "\d user;")     ;; postgres doesn't work: unsupported escape character \d

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
