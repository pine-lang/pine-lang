(ns pine.ast.group)

(defn handle [acc value]
  (assoc acc :group value))