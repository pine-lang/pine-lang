(ns pine.ast.main
  (:require [pine.parser :as parser]
            [pine.util :as util]
            [pine.ast.table :as table]
            [pine.ast.limit :as limit]))

(defn handle [state i {:keys [type value]}]
  (case type
    :table (table/handle state i value)
    :limit (limit/handle state i value)
    (update state :errors conj [i type "Unknown type"])))

(def init-state {:tables [] :columns [] :limit nil})

(defn generate [parse-tree]
  (util/reduce-indexed handle init-state parse-tree))

