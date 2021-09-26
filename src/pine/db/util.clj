(ns pine.db.util
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as s]))

(defn escape
  "Remove non alphanumeric chars from a string"
  [s]
  (s/replace s #"[^A-Za-z0-9_-]*" ""))

(defn exec
  "Execute raw sql queries"
  [connection query]
  (->> query
       (jdbc/query connection)))
