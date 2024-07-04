(ns pine.eval
  (:require [pine.db.main :as db]))

(defn q [s]
  (str "\"" s "\""))

(defn build-query [state]
  (let [{:keys [tables columns limit joins where aliases]} state
        from (or (when-let [{:keys [table alias]} (first tables)]
                   (str (q table) " AS " (q alias))) "?")
        join (when (not-empty (rest tables))
               (let [table-pairs (partition 2 1 tables)
                     join-statements (map (fn [[{from-alias :alias} {to-alias :alias to-table :table}]]
                                            (let [[a1 t1 _ a2 t2] (or
                                                                   (get-in joins [from-alias to-alias])
                                                                   (get-in joins [to-alias from-alias]))]
                                              (str "JOIN " (q to-table) " AS " (q to-alias)
                                                   " ON " (q a1) "." (q t1)
                                                   " = " (q a2) "." (q t2))))

                                          table-pairs)]
                 (clojure.string/join " " join-statements)))
        select (if (empty? columns)
                 "SELECT * FROM"
                 (str "SELECT " (clojure.string/join ", " (->> columns (map :column) (map q))) " FROM"))
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
