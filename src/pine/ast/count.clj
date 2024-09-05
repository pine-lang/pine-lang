(ns pine.ast.count)

;; todo: spec for the :value for a :limit

(defn handle [acc value]
  (assoc acc :count value))
