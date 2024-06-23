(ns pine.db.config)

(def connections {:dev {:host "localhost" ;; use for docker: "host.docker.internal"
                        :dbtype "postgres"
                        :dbname "?"
                        :user "?"
                        :password "?"
                        :schema nil}})
