(ns pine.data-types)

(defn- t [t x] {:type t
                :value x})
(defn string [x]
  (t :string x))

(defn number [x]
  (t :number x))
