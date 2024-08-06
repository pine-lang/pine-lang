(ns pine.core-dev
  (:require [pine.api :refer [app]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.reload :refer [wrap-reload]]))

(defn -main [& args]
  (run-jetty (wrap-reload #'app) {:port 33333 :join? false}))
