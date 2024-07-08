(ns pine.eval
  (:require [pine.db.main :as db]))

(defn q
  ([a b]
   (if a (str (q a) "." (q b)) (q b)))
  ([a]
   (str "\"" a "\"")))

(defn- build-join-query [{:keys [tables joins aliases]}]
  (when (not-empty (rest tables))
    (let [table-pairs (partition 2 1 tables)
          join-statements (map (fn [[{from-alias :alias} {to-alias :alias}]]
                                 (let [{to-table :table to-schema :schema} (get aliases to-alias)
                                       [a1 t1 _ a2 t2] (or
                                                        (get-in joins [from-alias to-alias])
                                                        (get-in joins [to-alias from-alias]))]
                                   (str "JOIN " (q to-schema to-table) " AS " (q to-alias)
                                        " ON " (q a1 t1)
                                        " = " (q a2 t2))))
                               table-pairs)]
      (clojure.string/join " " join-statements))))

(defn build-select-query [state]
  (let [{:keys [tables columns limit where context aliases]} state
        from (let [{a :alias} (first tables)
                   {table :table schema :schema} (get aliases a)]
               (str (q schema table) " AS " (q a)))
        join (build-join-query state)
        select (if (empty? columns)
                 (str "SELECT " context ".* FROM")
                 (str "SELECT " (clojure.string/join ", " (->> columns (map :column) (map q))) " FROM"))
        where-clause (when (not-empty where)
                       (str "WHERE "
                            (clojure.string/join " AND "
                                                 (for [[a col op val] where]
                                                   (str (q a col) " " op " ?")))))
        limit (when limit
                (str "LIMIT " limit))
        query (clojure.string/join " " (filter some? [select from join where-clause limit]))
        params (when (not-empty where)
                 (mapv (fn [[a col op val]] val) where))]
    {:query query :params params}))

(defn build-delete-query [state]
  (let [{:keys [column]} (state :delete)
        {:keys [query params]} (build-select-query state)]
    {:query (str "DELETE FROM x WHERE " (q column) " IN ( "  query " ) as x")
     :params params}))

(defn build-query [state]
  (let [{:keys [type]} (state :operation)]
    (cond
      (= type :delete) (build-delete-query state)
      :else (build-select-query state))))

(defn formatted-query [{:keys [query params]}]
  (let [replacer (fn [s param]
                   (let [param-str (if (string? param)
                                     (str "'" param "'")
                                     (str param))]
                     (clojure.string/replace-first s "?" param-str)))]
    (reduce replacer query params)))

(defn run-query [state]
  (let [connection-id (state :connection-id)]
    (->> state
         build-query
         (db/run-query connection-id))))
