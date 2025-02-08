(ns pine.core
  (:require [pine.api :refer [app]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

#_{:clj-kondo/ignore [:unused-binding]}
(defn -main [& args]
  (run-jetty app {:port 33333 :join? false}))

