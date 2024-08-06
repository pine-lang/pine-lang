(ns pine.core
  (:require [pine.api :refer [app]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(defn -main [& args]
  (run-jetty app {:port 33333 :join? false}))

