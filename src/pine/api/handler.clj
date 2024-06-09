(ns pine.api.handler
  (:require
   [compojure.core :refer :all]
   [compojure.route :as route]
   [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
   [ring.middleware.json :refer [wrap-json-params wrap-json-response]]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.util.response :refer [response]]

   [pine.parser :as parser]
   [pine.ast.main :as ast]
   [pine.eval.sql :as sql]))

(defn build-response [expression]
  (->> expression
       parser/parse
       ast/generate
       sql/generate))

(defn get-connection-metadata []
  ;; Implement this function
  {:message "get-connection-metadata not implemented"})

(defn wrap-logger
  [handler]
  (fn [request]
    (let [response (handler request)]
      (when (= 404 (:status response))
        (prn (format "Path not found: %s" (:uri request))))
      response)))

(defroutes app-routes
  (POST "/api/v1/build" [expression] (->> expression build-response response))
  (GET "/api/v1/connection" [] (-> (get-connection-metadata) response))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-json-params
      wrap-json-response
      wrap-logger
      (wrap-defaults api-defaults)
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put])))
