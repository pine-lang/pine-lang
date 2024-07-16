(ns pine.hints)

(defn- filter-relations [token candidates]
  "Given a partial token (which can be completed to a table) and all the
  candidate relations, we filter out the unrelated candidates"
  (filter #(-> %1
               first
               clojure.string/lower-case
               (clojure.string/includes? token)) candidates))

;; ---------------------------------------------------------------------------
;; Table Hints
;; ---------------------------------------------------------------------------

(defn create-hint-from-table [state tables]
  (let [refs (-> state :references :table)]
    (mapcat identity
            (for [table tables
                  :let [schemas (->> table refs :in keys)]]
              (for [schema schemas]
                {:schema schema :table table})))))

(defn table-hints [state token]
  "No context - get all the tables matching the token"
  (let [candidates (-> state :references :table)
        suggestions (filter-relations token candidates)]
    (create-hint-from-table state (->> suggestions keys distinct (sort-by count)))))

;; ---------------------------------------------------------------------------
;; Relation Hints
;; ---------------------------------------------------------------------------

(defn- filter-candidates [token candidates]
  "Given a partial token (which can be completed to a table) and all the
  candidates, we filter out only the unrelated candidates"
  (let [xs (filter #(clojure.string/includes? (clojure.string/lower-case %1) token) candidates)]
    (->> xs
         distinct
         (sort-by count))))

(defn- create-hint-from-relations [relations direction]
  (map (fn [relation]
         (let [table (first relation)
               via (get-in relation [1 :via])
               via-details (first (first (vals via)))
               direction (nth via-details 3)
               schema (nth via-details (if (= direction :refers-to) 4 0))
               column (nth via-details 2)]
           {:schema schema
            :table table
            :column column}))
       relations))

(defn relation-hints [state token]
  (let [from-alias (state :context)
        from-table (-> state :aliases (get from-alias) :table)
        parents (-> state :references :table (get from-table) :refers-to)
        children (-> state :references :table (get from-table) :referred-by)
        suggestions (filter-relations token (concat parents children))]
    (create-hint-from-relations suggestions :parent)))

(defn generate [state {token :table}]
  (if (nil? (state :context))

    ;; This is the first table - get all the tables matching the token
    (table-hints state token)

    ;; This is not the first table, then filter out the related tables
    (relation-hints state token)))
