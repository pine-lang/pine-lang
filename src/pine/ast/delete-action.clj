(ns pine.ast.delete-action)

(defn handle [state column]
  (assoc state :delete column))
