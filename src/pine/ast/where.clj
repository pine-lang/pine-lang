(ns pine.ast.where)

(defn handle [state [column operator value]]
  (let [a (state :current)
        [alias col] (:value column)
        alias (or alias a)]
    (update state :where conj [alias col operator value])))
