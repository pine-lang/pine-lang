(ns pine.utils)


;; https://groups.google.com/d/msg/clojure/7gCK7izwXBc/vWXYTycvPCwJ
(defn flip
  "Like partial except you supply everything but the first argument."
  ([f b] (fn [a] (f a b)))
  ([f b c] (fn [a] (f a b c)))
  ([f b c d & more]
   (fn [a] (apply f a b c d more))))
