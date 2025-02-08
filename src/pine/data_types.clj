(ns pine.data-types)

(defn- t [t x] {:type t
                :value x})
(defn string [x]
  (t :string x))

(defn number [x]
  (t :number x))

#_{:clj-kondo/ignore [:redefined-var]}
(defn symbol [x]
  (t :symbol x))

(defn column
  ([column] (t :column [nil column]))
  ([alias column] (t :column [alias column])))
