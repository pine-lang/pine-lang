(ns pine.ast.limit)

;; todo: spec for the :value for a :limit

(defn handle [acc i value]
  (assoc acc :limit value))
