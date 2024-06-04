(ns pine.ast.table)

;; todo: spec for the :value for a :table

(defn handle [acc i {:keys [table alias]}]
  (update acc :tables conj {:table table :alias (or alias (str table "_" i))}))
