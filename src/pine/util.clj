(ns pine.util)

(defn reduce-indexed [f acc coll]
  (reduce (fn [acc [index item]]
            (f acc index item))
          acc
          (map-indexed vector coll)))
