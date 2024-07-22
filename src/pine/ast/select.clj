(ns pine.ast.select)

(defn handle [state value]
  (let [context (state :context)
        columns (map #(if (:alias %1) %1 (assoc %1 :alias context)) value)]
    (-> state
        (update :columns into columns))))
