(ns pine.hints)

;; In the current version, I'll only support suggestions for table names and the schema will be autocompleted
;; In the coming versions, I'll add support for alias suggestion

(defn- filter-candidates [token candidates]
  "Given a list of a partial token (which can be completed to a table) and all
  the candidates, we filter out only the relevant candidates"
  (let [xs (filter #(clojure.string/includes? (clojure.string/lower-case %1) token) candidates)]
    (->> xs
         distinct
         (sort-by count))))

(defn qualify-tables [state tables]
  (let [refs (-> state :references :table)]
    (mapcat identity
            (for [table tables
                  :let [schemas (->> table refs :in keys)]]
              (for [schema schemas]
                {:schema schema :table table})))))

(defn table-hints [state token]
  "Update the suggestions based on the candidate for the table name"
  (if (nil? (state :context))

    ;; This is the first table - get all the tables matching the token
    (let [candidates (-> state :references :table keys)
          suggestions (filter-candidates token candidates)]
      (qualify-tables state suggestions))

;; This is not the first table, then filter out the related tables
    (let [from-alias (state :context)
          from-table (-> state :aliases (get from-alias) :table)
          left-candidates (-> state :references :table (get from-table) :refers-to keys)
          right-candidates (-> state :references :table (get from-table) :referred-by keys)
          candidates (concat left-candidates right-candidates)
          suggestions (filter-candidates token candidates)]
      (qualify-tables state suggestions))))

(defn generate [state value]
  (let [token                (value :table)
        hints                (table-hints state token)]
    hints))
