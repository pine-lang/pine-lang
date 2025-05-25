(ns pine.ast.group)

(defn handle [state value]
  (let [i (state :index)
        current (state :current)
        columns (map #(-> %1
                          (assoc :alias (or (:alias %1) current))
                          (assoc :index i))
                     (:columns value))
        ;; TODO: This needs to return a column that shouldn't be quoted
        fn-columns (map (fn [name] {:alias nil :column (str name "(1)")}) (:functions value))]
    (-> state
        (assoc :columns (into fn-columns columns)) ;; set the columns to be aggregated
        (update :group into columns))))

