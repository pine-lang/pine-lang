(ns pine.api
  (:require
   [compojure.core :refer :all]
   [compojure.route :as route]
   [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
   [ring.middleware.json :refer [wrap-json-params wrap-json-response]]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.util.response :refer [response]]

   [pine.parser :as parser]
   [pine.ast.main :as ast]
   [pine.eval :as eval]
   [pine.db.main :as db]
   [pine.util :as util]
   ;; Encode arrays and json results in API responses
   [cheshire.generate :refer [add-encoder encode-str]]))

;; array/json encoding
(add-encoder org.postgresql.util.PGobject encode-str)
(add-encoder org.postgresql.jdbc.PgArray encode-str)

(def version "0.12.0")

(defn- generate-state [expression]
  (let [{:keys [result error]} (->> expression parser/parse)]
    (if result {:result (-> result ast/generate)}
        {:error-type "parse"
         :error error})))

(defn api-build [expression]
  (let [connection-name (util/get-connection-name @db/connection-id)]
    (try
      (let [result (generate-state expression)
            {state :result error :error} result]
        (if error result
            {:connection-id connection-name
             :version version
             :query (-> state eval/build-query eval/formatted-query)
             :ast (dissoc state :references :join-map)
             }))
      (catch Exception e {:connection-id connection-name
                          :error (.getMessage e)}))))

(defn api-eval [expression]
  (let [connection-name (util/get-connection-name @db/connection-id)]
    (try
      (let [result (generate-state expression)
            {state :result error :error} result]
        (if error result
            {:connection-id connection-name
             :version version
             :time (db/run-query (state :connection-id) {:query "SELECT NOW() AT TIME ZONE 'UTC' AS utc, NOW() AS db;"})
             :result (eval/run-query state)}))
      (catch Exception e {:connection-id connection-name
                          :error (.getMessage e)}))))

(defn get-connection []
  (let [connection-id   @db/connection-id
        connection-name (util/get-connection-name connection-id)
        _               (db/init-references @db/connection-id)]
    {:result
     {:connection-id connection-name
      :version version}}))

(defn wrap-logger
  [handler]
  (fn [request]
    (let [response (handler request)]
      (when (= 404 (:status response))
        (prn (format "Path not found: %s" (:uri request))))
      response)))

(defroutes app-routes
  (POST "/api/v1/build" [expression] (->> expression api-build response))
  (GET "/api/v1/connection" [] (-> (get-connection) response))
  (POST "/api/v1/eval" [expression] (->> expression api-eval response))
  ;; pine-mode.el
  (POST "/api/v1/build-with-params" [expression] (->> expression api-build :query response))
  ;; default case
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-json-params
      wrap-json-response
      wrap-logger
      (wrap-defaults api-defaults)
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put])))
