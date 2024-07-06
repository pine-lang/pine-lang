(ns pine.ast.where)

(defn handle [acc [column operator value]]
  (assoc acc :where [column operator value]))
