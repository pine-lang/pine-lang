(ns pine.ast.where)

;; todo: spec for the :value for a :limit

(defn handle [acc value]
  (assoc acc :where value))
