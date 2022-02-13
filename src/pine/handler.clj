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

            [pine.db.protocol :as protocol]))

;; (reset! db/connection (db/get-connection :mysql-test))

(defn prepare [expression]
  (let [schema (protocol/get-schema @db/connection)]
    (pine/pine-prepare schema expression)))

(defn build
  "Build the query with with the params filled in"
  [prepared]
  (let [query    (:query prepared)
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
                                             :string (db/quote-string x) ;; TODO: qouting shouldn't happen here
                                             (format "%s" x)
                                             )))
                      )
        ]
    (str
     "\n"
     (reduce (fn [acc param] (s/replace-first acc "?" param)) (:query prepared) params) ";"
     "\n")
    ))

(defn- api-build [request]
  (->> (get-in request [:params "expression"])
       ((fn [x] (prn x) x))
       prepare
       build
       response
       ))

(defn- api-eval [request]
  (->> (get-in request [:params "expression"])
       prepare
       pine/pine-eval
       response
       ))

(defroutes app-routes
  (POST "/pine/build" request api-build) ;; backwads compat
  (POST "/build" request api-build)
  (POST "/eval" request api-eval)
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-json-params
      wrap-json-response
      (wrap-defaults api-defaults)
      ))
