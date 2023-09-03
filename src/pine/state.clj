(ns pine.state
  (:require
   ;; only for testing with the repl
   [pine.db.connection :as connection]))

(def c "Connection" (atom nil))

;; for testing with the repl
(defn md []
  (connection/get-metadata @c))
