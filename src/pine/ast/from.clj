(ns pine.ast.from)

;; todo: spec for the :value for a :limit

(defn handle [state value]
  (-> state
      ;; (assoc :context (state :current))
      (assoc :context (:alias value))
      (assoc :current (:alias value))))
