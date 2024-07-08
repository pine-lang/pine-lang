(ns pine.ast.where)

(defn handle [state [column operator value]]
  (let [a (state :context)]
    (update state :where conj [a column operator value])))
