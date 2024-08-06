(ns pine.ast.limit)

;; todo: spec for the :value for a :limit

(defn handle [acc value]
  (assoc acc :limit value))
