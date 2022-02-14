(defproject pine "0.1.0-SNAPSHOT"
  :description "Pine : A convenient way to query relational data"
  :url "https://github.com/ahmadnazir/pine"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.mchange/c3p0 "0.9.5.2"] ;; connection pooling
                 [compojure "1.5.1"]
                 [instaparse "1.4.9"]
                 [org.clojure/clojure "1.10.3"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [ring/ring-defaults "0.2.1"]
                 [ring/ring-json "0.4.0"]

                 ;; database
                 [org.clojure/java.jdbc "0.7.3"]
                 [mysql/mysql-connector-java "5.1.18"]
                 [org.postgresql/postgresql "42.2.24.jre7"]
                 ]
  :plugins [[lein-ring "0.9.7"]
            ]
  :ring {:handler pine.handler/app}
  :profiles
  {
   :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}
   :repl {
          :plugins [[cider/cider-nrepl "0.26.0"]
                    [refactor-nrepl "2.5.1"] ]
          :dependencies [[nrepl/nrepl "0.8.3"]]
          }
   }
  ;; :aot :all
  ;; https://github.com/technomancy/leiningen/blob/master/sample.project.clj
  :repl-options {
                 :welcome (println "\nRepl for pine :: warming up ... \n")
                 :host "0.0.0.0"
                 :port "33333"
                 }
  )

