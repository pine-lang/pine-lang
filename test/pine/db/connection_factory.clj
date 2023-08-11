(ns pine.db.connection-factory
  (:require [pine.db.connection :as sut]
            [clojure.test :as t]
            [pine.fixtures.mysql :as fixtures-mysql]
            [pine.fixtures.postgres :as fixtures-postgres]
            ))


(defn create [type]
  (case type
    :postgres (pine.db.postgres.Postgres. "dummy" {
                                                   :fixtures { :relations fixtures-postgres/relations }
                                                   :schema fixtures-postgres/schema
                                                   })
    :mysql (pine.db.mysql.Mysql. "dummy" {
                                          :fixtures { :relations (throw (Exception. "Fixtures for relations not supported for mysql")) }

                                          :schema fixtures-mysql/schema})
    (throw (Exception. "Unknown type"))))



