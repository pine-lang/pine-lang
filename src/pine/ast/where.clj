(ns pine.ast.where)

(defn handle [state [column operator value]]
  (let [a (state :context)]
    (assoc state :where [a column operator value])))
