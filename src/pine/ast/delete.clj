(ns pine.ast.delete)

(defn handle [state column]
  (assoc state :delete column))
