(ns pine.db.connection-factory
  (:require [pine.db.connection :as sut]
            [clojure.test :as t]
            [pine.fixtures.mysql :as fixtures-mysql]
            [pine.fixtures.postgres :as fixtures-postgres]
            ))


(defn create [type]
  (case type
    :postgres (pine.db.postgres.Postgres. "dummy" {:schema fixtures-postgres/schema})
    :mysql (pine.db.mysql.Mysql. "dummy" {:schema fixtures-mysql/schema})
    (throw (Exception. "Unknown type"))))
