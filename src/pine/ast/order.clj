(ns pine.ast.order)

(defn handle [state value]
  (let [current (state :current)
        columns (map #(assoc %1 :alias current) value)]
    (-> state
        (update :order into columns))))
