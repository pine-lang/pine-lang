(ns pine.eval
  (:require [pine.db.main :as db]))

(defn q [s]
  (str "\"" s "\""))

(defn build-query [state]
  (let [{:keys [tables columns limit joins where aliases]} state
        from (when (seq tables)
               (let [{:keys [table alias]} (first tables)]
                 (str "FROM " (q table) " AS " (q alias))))
        join (->> tables
                  (mapcat (fn [{:keys [alias]}]
                            (map (fn [[to-table [to-alias to-col _ from-alias from-col]]]
                                   (str "JOIN " (q (:table (get aliases to-alias))) " AS " (q to-alias)
                                        " ON " (q from-alias) "." (q from-col)
                                        " = " (q to-alias) "." (q to-col)))
                                 (get joins alias))))
                  (clojure.string/join " "))
        select (if (empty? columns)
                 "SELECT *"
                 (str "SELECT " (clojure.string/join ", " (map q columns))))
        where-clause (when where
                       (let [[col op val] where]
                         (str "WHERE " (q col) " " op " ?")))
        limit (when limit
                (str "LIMIT " limit))
        query (clojure.string/join " " (filter some? [select from join where-clause limit]))
        params (when where
                 [(nth where 2)])]
    {:query query :params params}))

(defn run-query [state]
  (let [connection-id (state :connection-id)]
    ;; connection-id
    (->> state
         build-query
         (db/run-query connection-id))))
