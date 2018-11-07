(defproject pine "0.1.0-SNAPSHOT"
  :description "Pine : A convenient way to query relational data"
  :url "https://github.com/ahmadnazir/pine"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/java.jdbc "0.7.3"]
                 [mysql/mysql-connector-java "5.1.18"]
                 [com.mchange/c3p0 "0.9.5.2"] ;; connection pooling
                 [compojure "1.5.1"]
                 [ring/ring-defaults "0.2.1"]
                 [ring/ring-json "0.4.0"]
                 [instaparse "1.4.9"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 ]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler pine.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}}
  :aot :all
  )

