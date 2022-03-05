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

(defn- api-build [expression]
  (->> expression
       ((fn [x] (prn x) x))
       prepare
       build
       ))

(defn- connection-id []
  (protocol/get-connection-id @db/connection))

(defn- api-eval [expression]
  (->> expression
       prepare
       pine/pine-eval
       (assoc {} :connection-id (connection-id) :result)
       response
       ))

(defn- set-connection [id]
  (if-let [connections ((db/get-connections) id)]
    (do
      (->> id
           db/get-connection
           (reset! db/connection))
      {:connection-id (connection-id)})
    {:error (format "Connection '%s' does not exist" id)}
    ))

(defn- get-connections []
  (->> (db/get-connections)
       keys
       (map name)))

(defroutes app-routes
  (POST "/pine/build" [expression] (->> expression api-build response)) ;; backwads compat
  (POST "/build" [expression] (->> expression api-build (assoc {} :connection-id (connection-id) :query) response))
  (POST "/eval" [expression] (api-eval expression))
  (PUT "/connection" [connection-id] (->> connection-id keyword set-connection response))
  (GET "/connections" [] (->> (get-connections) (assoc {} :result) response))
  (route/not-found "Not Found"))

;; DEBUG
;; (set-connection :default)

(def app
  (do
    (prn (format "Connection: [%s]" (protocol/get-connection-id @db/connection)))
    (-> app-routes
        wrap-json-params
        wrap-json-response
        (wrap-defaults api-defaults))))
