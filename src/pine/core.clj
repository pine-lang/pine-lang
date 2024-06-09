(ns pine.core
  (:require [pine.api.handler :refer [app]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(defn -main [& args]
  (run-jetty app {:port 33333 :join? false}))

