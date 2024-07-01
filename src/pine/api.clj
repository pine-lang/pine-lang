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
   [pine.db.main :as db]))

(def version "0.5.0")

(defn api-build [expression]
  (let [connection-id @db/connection-id]
    (try
      (let [state (->> expression
                       parser/parse
                       ast/generate)]
        {:connection-id connection-id
         :version version
         :query (:query (eval/build-query state))
         :state (dissoc state :references)
         ;; Backwards compatibility
         ;; Instead of the following, please use `state`.
         :deprecation-notice "Properties will be removed in the next major version: `hints`, `context`. Use `state` instead."
         :hints (state :hints)
         :context (let [tables (state :tables)
                        type (-> state :operation :type)]
                    (if
                     (= type :table)
                      (-> tables reverse rest reverse)
                      tables))})

      (catch Exception e {:connection-id connection-id
                          :error (.getMessage e)}))))

(defn api-eval [expression]
  (let [state (->> expression
                   parser/parse
                   ast/generate)]
    {:connection-id @db/connection-id
     :version version
     :result (eval/run-query state)}))

(defn get-connection-metadata []
  {:result
   {:connection-id @db/connection-id
    :version version
    ;; TODO for backwards compatibility wrap the references in the same shape as the old version
    :metadata {:db/references (@db/references @db/connection-id)}}})

(defn wrap-logger
  [handler]
  (fn [request]
    (let [response (handler request)]
      (when (= 404 (:status response))
        (prn (format "Path not found: %s" (:uri request))))
      response)))

(defroutes app-routes
  (POST "/api/v1/build" [expression] (->> expression api-build response))
  (GET "/api/v1/connection" [] (-> (get-connection-metadata) response))
  (POST "/api/v1/eval" [expression] (->> expression api-eval response))
  (route/not-found "Not Found"))

(db/init-references @db/connection-id)

(def app
  (-> app-routes
      wrap-json-params
      wrap-json-response
      wrap-logger
      (wrap-defaults api-defaults)
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put])))
