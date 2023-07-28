(ns pine.db
  (:require [pine.config :as config]
            [pine.db.mysql :as mysql ]
            [pine.db.postgres :as postgres]
            [pine.db.connection :as connection]
            [pine.state :as state])
  )

(defn get-connections [] (config/config :connections))
(defn get-connection [id] (let [config ((get-connections) id)
                                type (config :dbtype)]
                            (cond (= type "mysql") (pine.db.mysql.Mysql. id config)
                                  (= type "postgres") (pine.db.postgres.Postgres. id config)
                                  :else (throw (Exception. (format "Db not supported: %s" type))))))

;; DB wrappers
;; (defn- qt [x]
;;   (connection/quote @state/c (name x)))
;; (defn quote
;;   ([x] (qt x))
;;   ([x y]
;;    (cond (not (nil? x)) (format "%s.%s" (qt x) (qt y))
;;          :else (qt y))))

(defn quote-string [x]
  (connection/quote-string @state/c x))

(defn references
  ([schema table]
   (connection/references @state/c table))
  ([table]
   (let [schema (connection/get-schema @state/c)]
     (connection/references @state/c table))))

(defn get-columns
  "Returns the list of columns a table has"
  [connection table-name]
  (connection/get-columns connection table-name))

;; Helpers

(defn $
  "Execute raw sql queries:
  ($ \"select id from users limit 10\")
  ($ first \"select id from users limit 10\")
  "
  ([fn query]
   (->> query
        (connection/query @state/c)
        fn))
  ([query]
   ($ identity query)))

(defn $!
  "Execute non select queries:
  ($ \"use tmp\")
  "
  ([fn query]
   (->> query
        (connection/execute! @state/c)
        fn))
  ([query]
   ($! identity query)))

;; ($ "show tables")
;; ($! "use tmp")
