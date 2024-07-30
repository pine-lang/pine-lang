(ns pine.eval
  (:require [pine.db.main :as db]))

(defn q
  ([a b]
   (if a (str (q a) "." (q b)) (q b)))
  ([a]
   (str "\"" a "\"")))

(defn- build-join-clause [{:keys [tables joins aliases]}]
  (when (not-empty (rest tables))
    (let [join-statements (map (fn [[from-alias to-alias relation]]
                                 (let [[a1 t1 _ a2 t2] relation
                                       {to-table :table to-schema :schema} (get aliases to-alias)]
                                   (str "JOIN " (q to-schema to-table) " AS " (q to-alias)
                                        " ON " (q a1 t1)
                                        " = " (q a2 t2))))
                               ;; (reverse joins)
                               joins)]
      (clojure.string/join " " join-statements))))

(defn- build-columns-clause [{:keys [columns current]}]
  (let [column-context (if (empty? columns) nil (-> columns last :alias))
        select-all (cond
                     (nil? column-context) (str current ".*")
                     (= current column-context) ""
                     :else (str ", " current ".*"))]
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
  (let [{:keys [tables columns limit where aliases]} state
        from         (let [{a :alias} (first tables)
                           {table :table schema :schema} (get aliases a)]
                       (str (q schema table) " AS " (q a)))
        join         (build-join-clause state)
        select       (build-columns-clause state)
        where-clause (when (not-empty where)
                       (str "WHERE "
                            (clojure.string/join " AND "
                                                 (for [[a col op val] where]
                                                   (if (= op "in")
                                                     (str (q a col) " IN (" (clojure.string/join ", " (repeat (count val) "?"))  ")")
                                                     (str (q a col) " " op " ?"))))))
        limit (str "LIMIT " (or limit 250))
        query (clojure.string/join " " (filter some? [select from join where-clause limit]))
        params (when (not-empty where)
                 (mapcat (fn [[a col op val]] (if (coll? val) val [val])) where))]
    {:query query :params params}))

(defn build-delete-query [state]
  (let [{:keys [delete current aliases]} state
        {table :table schema :schema}     (get aliases current)
        {:keys [column]}                  delete
        state                             (assoc state :columns [{:column column :alias current}])
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
