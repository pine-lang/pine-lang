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

(defn- generate-expression [{:keys [schema table column]}]
  (str (if schema (str schema ".") "") table (if column (str " ." column) "")))

;; via-details look like:
;; ["z"  "document"      "created_by"  :refers-to   "y"  "employee" "id"]
(defn- create-hint-from-relation-array [table via-details]
  (map (fn [vd]
         (let [direction (nth vd 3)
               child? (= direction :refers-to)
               schema (nth vd (if child? 4 0))
               column (nth vd 2)
               ;; child? (= column "tenant_id")
               ]
           {:schema schema
            :table table
            :column column
            ;; TODO: We need directionality here!
            ;; :child child?
            }))
       via-details))

(defn- create-hint-from-relations [relations]
  (mapcat (fn [relation]
            (let [table (first relation)
                  via (get-in relation [1 :via])
                  via-details (mapcat identity (vals via))]
              (create-hint-from-relation-array table via-details)))
          relations))

(defn relation-hints [state token]
  (let [from-alias (state :context)
        from-table (-> state :aliases (get from-alias) :table)
        parents (-> state :references :table (get from-table) :refers-to)
        children (-> state :references :table (get from-table) :referred-by)
        suggestions (filter-relations token (concat parents children))]
    (-> suggestions
        create-hint-from-relations
        ;; TODO: instead of doing a distinct, we can do a reduce and keep track
        ;; of duplicates
        distinct)))

(defn generate [state {token :table}]
  (let [table-hints         (if (nil? (state :context))

                              ;; This is the first table - get all the tables matching the token
                              (table-hints state token)

                              ;; This is not the first table, then filter out the related tables
                              (relation-hints state token))
        add-pine-expression (fn [h] (assoc h :pine (generate-expression h)))]
    {:table (->>  table-hints
                  (map add-pine-expression)
                  ;; (map-indexed (fn [i x] (assoc x :child (if (< i 8) true false))))
                  )}))
