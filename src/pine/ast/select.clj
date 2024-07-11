(ns pine.ast.select)

(defn handle [state value]
  (let [context (state :context)
        columns (map #(assoc %1 :alias context) value)]
    (-> state
        (update :columns into columns))))
