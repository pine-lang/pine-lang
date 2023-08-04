(ns pine.api
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [pine.core :as pine]
            [pine.db :as db]
            [pine.state :as state]
            [pine.config :as config]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-params wrap-json-response]]
            [ring.middleware.cors :refer [wrap-cors]]
            [clojure.string :as s]
            [cheshire.core :refer [encode]]
            [cheshire.generate :refer [add-encoder encode-str remove-encoder]]
            [pine.db.connection :as connection]))

(add-encoder org.postgresql.util.PGobject encode-str)
(add-encoder org.postgresql.jdbc.PgArray encode-str)

;; (defn prepare [connection expression]
;;   (let [schema (connection/get-schema @state/c)]
;;     (pine/pine-prepare schema expression)))

(defn build
  "Build the query with with the params filled in"
  [connection prepared]
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
                                             :string (connection/quote-string connection x)
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
       (pine/pine-prepare @state/c)
       (build @state/c)
       ))

(defn- connection-id []
  (connection/get-connection-id @state/c))

(defn- api-build-response [expression]
  (let [id (connection-id)]
    (try
      (let [prepared (pine/pine-prepare @state/c expression)
            hints (pine/pine-hint @state/c expression)]
        {
         :connection-id id
         :query (prepared :query)
         :params (prepared :params)
         :hints hints
         })
      (catch Exception e {
                          :connection-id id
                          :error (.getMessage e)
                          })
      )))

(defn- api-eval [expression]
  (let [
        id (connection-id)]
    (try
      (let [prepared (pine/pine-prepare @state/c expression)
            query (prepared :query)
            params (prepared :params)
            ]
        (encode
         {
          :connection-id id
          :query query
          :params params
          :result (pine/pine-eval @state/c prepared)
          }))
      (catch Exception e {:connection-id id
                          :error (.getMessage e)})
      )
    )
  )

(defn set-connection [id]
  (if-let [connections ((db/get-connections) id)]
    (do
      (->> id
           db/get-connection
           (reset! state/c))
      {:connection-id (connection-id)})
    {:error (format "Connection '%s' does not exist" id)}
    ))

(defn- get-connections []
  (->> (db/get-connections)
       keys
       (map name)))

(defroutes app-routes
  (POST "/pine/build" [expression] (->> expression api-build response)) ;; backwads compat
  (POST "/build" [expression] (->> expression api-build-response response))
  (POST "/eval" [expression] (->> expression api-eval response))
  (PUT "/connection" [connection-id] (->> connection-id keyword set-connection response))
  (GET "/connection" [] (->> (connection-id) (assoc {} :result) response))
  (GET "/connections" [] (->> (get-connections) (assoc {} :result) response))
  (route/not-found "Not Found"))

(if (config/config :connect-on-start)
  (do
    (set-connection (config/config :connection-id))
    (->> "company | l: 1" api-build)
    )
  )

(def app
  (do
    (-> app-routes
        wrap-json-params
        wrap-json-response
        (wrap-defaults api-defaults)
        (wrap-cors :access-control-allow-origin [#".*"] :access-control-allow-methods [:get :post])
        )))
