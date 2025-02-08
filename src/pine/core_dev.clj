(ns pine.core-dev
  (:require [pine.api :refer [app]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.reload :refer [wrap-reload]]))

#_{:clj-kondo/ignore [:unused-binding]}
(defn -main [& args]
  (run-jetty (wrap-reload #'app) {:port 33333 :join? false}))
