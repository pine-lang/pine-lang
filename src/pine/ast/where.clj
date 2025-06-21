(ns pine.ast.where)

(defn handle [state [column operator value]]
  (let [a (state :current)
        [alias col cast] (:value column)
        alias (or alias a)]
    (update state :where conj [alias col cast operator value])))
