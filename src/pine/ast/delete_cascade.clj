(ns pine.ast.delete-cascade)

(defn handle [state column]
  (assoc state :delete-cascade column))
