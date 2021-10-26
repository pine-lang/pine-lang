(ns pine.db
  (:require [clojure.java.jdbc :as jdbc]
            [pine.config :as c]
            ;; [pine.db.mysql :as dbadapter]
            [pine.db.postgres :as dbadapter]
            )
  )

;; (ns-unalias 'pine.db 'dbadapter)


;; DB wrappers

;; TODO: move to the adapter
;; (def connection-delay (delay (dbadapter/pool c/config))) ;; mysql
(def connection-delay (delay c/config))              ;; postgres

(defn quote [x]
  (dbadapter/quote x))

(defn quote-string [x]
  (dbadapter/quote-string x))

(defn relation
  "Get the column that has the relationship between the tables:
  (relation :caseFiles: :owns :documents:) => \"caseFileId\"
  (relation :documents: :owned-by :caseFile:) => \"caseFileId\"
  "
  [schema t1 relationship t2]
  (prn t1)
  (case relationship
    :owns     (t1 (dbadapter/references schema t2))
    :owned-by (t2 (dbadapter/references schema t1))
    :else     nil)
  )

(defn get-columns
  "Returns the list of columns a table has"
  [schema table-name]
  (dbadapter/get-columns schema table-name))


(defn get-schema'
  [config]
  (dbadapter/get-schema config))

(def get-schema (memoize get-schema'))

;; DB connection

(defn connection [] @connection-delay)

;; (jdbc/query (connection) "show tables;") ;; mysql
;; (jdbc/query (connection) "\d user;")     ;; postgres

;; Helpers

(defn $
  "Execute raw sql queries:
  ($ \"select id from users limit 10\")
  ($ first \"select id from users limit 10\")
  "
  ([fn query]
   (->> query
        (jdbc/query (connection))
        fn))
  ([query]
   ($ identity query)))

(defn $!
  "Execute non select queries:
  ($ \"use tmp\")
  "
  ([fn query]
   (->> query
        (jdbc/execute! (connection))
        fn))
  ([query]
   ($! identity query)))

;; ($ "show tables")
;; ($! "use tmp")
