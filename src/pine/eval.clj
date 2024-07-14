(ns pine.eval
  (:require [pine.db.main :as db]))

(defn q
  ([a b]
   (if a (str (q a) "." (q b)) (q b)))
  ([a]
   (str "\"" a "\"")))

(defn- build-join-clause [{:keys [tables joins aliases]}]
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

(defn- build-columns-clause [{:keys [columns context]}]
  (let [column-context (if (empty? columns) nil (-> columns last :alias))
        select-all (cond
                     (nil? column-context) (str context ".*")
                     (= context column-context) ""
                     :else (str ", " context ".*"))]
    (str
     "SELECT "
     (clojure.string/join
      ", "
      (map (fn [{:keys [column alias column-alias]}]
             (let [c (q alias column)]
               (if column-alias (str c " AS " (q column-alias)) c))) columns))
     select-all
     " FROM")))

(defn build-select-clause [state]
  (let [{:keys [tables columns limit where context aliases]} state
        from (let [{a :alias} (first tables)
                   {table :table schema :schema} (get aliases a)]
               (str (q schema table) " AS " (q a)))
        join (build-join-clause state)
        select (build-columns-clause state)
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
  (let [{:keys [delete context aliases]} state
        {table :table schema :schema}     (get aliases context)
        {:keys [column]}                  delete
        state                             (assoc state :columns [{:column column :alias context}])
        {:keys [query params]}            (build-select-clause state)]
    {:query (str "DELETE FROM " (q schema table) " WHERE " (q column) " IN ( "  query " )")
     :params params}))

(defn build-query [state]
  (let [{:keys [type]} (state :operation)]
    (cond
      (= type :delete) (build-delete-query state)
      :else (build-select-clause state))))

(defn formatted-query [{:keys [query params]}]
  (let [replacer (fn [s param]
                   (let [param-str (if (string? param)
                                     (str "'" param "'")
                                     (str param))]
                     (clojure.string/replace-first s "?" param-str)))]
    (str "\n" (reduce replacer query params) ";\n")))

(defn run-query [state]
  (let [connection-id (state :connection-id)]
    (->> state
         build-query
         (db/run-query connection-id))))
