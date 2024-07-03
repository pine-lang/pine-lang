(ns pine.ast.select)

(defn handle [state value]
  (-> state
      (assoc :columns value)))
