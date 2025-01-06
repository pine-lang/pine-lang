(ns pine.ast.hints
  (:require [clojure.string :as str]))

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

(defn- generate-expression [{:keys [schema table column parent]}]
  (str (if parent "of: " "") (if schema (str schema ".") "") table (if column (str " ." column) "")))

;; via-details look like:
;; ["z"  "document"      "created_by"  :refers-to   "y"  "employee" "id"]
(defn- create-hint-from-relation-array [table via-details]
  (map (fn [vd]
         (let [direction (nth vd 3)
               parent? (= direction :refers-to)
               schema (nth vd 4)
               column (nth vd (if parent? 2 6))]
           {:schema schema
            :table table
            :column column
            :parent parent?}))
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

(defn generate-table-hints [state]
  (let [{token :table parent :parent} (-> state :tables reverse first)
        from-alias (state :context)
        table-hints (if from-alias
                      ;; This is not the first table, then filter out the related tables
                      (relation-hints state token)
                      ;; This is the first table - get all the tables matching the token
                      (table-hints state token))
        ;; Filter by parent if specified
        table-hints (if parent
                      (filter #(= (:parent %) true) table-hints)
                      table-hints)
        ;; Add pine expression to each hint
        add-pine-expression (fn [h] (assoc h :pine (generate-expression h)))]
    (map add-pine-expression table-hints)))

(defn generate-all-column-hints [state a]
  (let [aliases (state :aliases)
        {table :table schema :schema} (->> a (get aliases))
        columns (if schema
                  (get-in state [:references :schema schema :table table :columns])
                  (get-in state [:references :table table :columns]))]
    (for [column columns]
      (-> column
          (select-keys [:column])
          (assoc :alias a)))))

(defn generate-column-hints [state]
  (let [column (some-> state :columns reverse first)
        a (if column (column :alias) (state :current))
        hints (generate-all-column-hints state a)]
    (if column
      ;; Filter the results to include columns that contain the value of :column
      (filter #(str/includes? (:column %) (:column column)) hints)
      ;; Else, return all hints
      hints)))

(defn handle [state]
  (let [type (-> state :operation :type)
        type (case type :select-partial :select type)
        hints (case type
                :table (generate-table-hints state)
                :select (generate-column-hints state)
                ;; for :where-partial, we can also use the :select hints as we need to see the columns
                [])
        ]
    (assoc-in state [:hints type] (or hints []))))
