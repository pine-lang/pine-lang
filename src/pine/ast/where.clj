(ns pine.ast.where)

(defn handle [state [column operator value]]
  (let [a (state :current)
        [alias col cast] (:value column)
        alias (or alias a)]
    (update state :where conj [alias col cast operator value])))

(defn handle-partial [state {:keys [complete-conditions partial-condition]}]
  ;; For WHERE-PARTIAL, we only store the complete conditions in :where
  ;; The partial condition is used for hints, not for query generation
  (let [a (state :current)]
    (reduce (fn [s condition]
              (let [[column operator value] (:value condition)
                    [alias col cast] (:value column)
                    alias (or alias a)]
                (update s :where conj [alias col cast operator value])))
            state
            complete-conditions)))
