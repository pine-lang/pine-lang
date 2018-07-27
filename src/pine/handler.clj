(ns pine.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [pine.core :as pine]
            [pine.db :as db]
            [pine.config :as c]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-params wrap-json-response]]
            [clojure.string :as s]
            ))

(defn build-query
  "Build the "
  [schema expression]
  (let [prepared (pine/pine-prepare schema expression)
        query    (:query prepared)
        params   (->> (:params prepared)
                      (map (fn [x] (s/replace x "\"" "\\\"")))
                      (map (partial format "\"%s\""))
                      )
        ]
    (str
     (reduce (fn [acc param] (s/replace-first acc "?" param)) (:query prepared) params) ";")
    ))

(defroutes app-routes
  (context "/pine/build" []
           (defroutes query-routes
             (POST "/" request
                   (->> (get-in request [:params "expression"])
                        ((fn [x] (prn x) x))
                        (build-query (db/schema c/db "penneo")) ;; TODO: shouldn't need to specify db
                        ;; ((fn [x] {:query x}))
                        response
                        )
                   )
             ))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-json-params
      wrap-json-response
      (wrap-defaults api-defaults)
      ))
