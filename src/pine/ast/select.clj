(ns pine.ast.select)

(defn handle [state value]
  (let [current (state :current)
        columns (map #(if (:alias %1) %1 (assoc %1 :alias current)) value)]
    (-> state
        (update :columns into columns))))
