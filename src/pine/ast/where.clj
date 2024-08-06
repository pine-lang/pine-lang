(ns pine.ast.where)

(defn handle [state [column operator value]]
  (let [a (state :current)]
    (update state :where conj [a column operator value])))
