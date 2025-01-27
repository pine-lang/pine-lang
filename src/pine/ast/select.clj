(ns pine.ast.select)

(defn handle [state value]
  (let [i       (state :index)
        current (state :current)
        columns (map #(-> %1
                          (assoc :alias (or (:alias %1) current))
                          (assoc :index i))
                     value)]
    (-> state
        (update :columns into columns))))
