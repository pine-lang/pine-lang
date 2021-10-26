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

                      ;; Quotes are not supported by the language
                      ;;
                      ;; Still, this function needs to be protected in case the
                      ;; params contain a quote
                      (map (fn [[t x]]
                             [t (-> x
                                    (s/replace "\"" "_")
                                    (s/replace "'"  "_")
                                    )
                              ]))

                      (map (fn [[t x]] (case t
                                             :string (db/quote-string x)
                                             (format "%s" x)
                                             )))
                      )
        ]
    (str
     "\n"
     (reduce (fn [acc param] (s/replace-first acc "?" param)) (:query prepared) params) ";"
     "\n")
    ))

(defroutes app-routes
  (context "/pine/build" []
           (defroutes query-routes
             (POST "/" request
                   (->> (get-in request [:params "expression"])
                        ((fn [x] (prn x) x))
                        (build-query (db/get-schema c/config)) ;; TODO: shouldn't need to specify db
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
