(ns pine.db.mysql-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.mysql :as mysql]
            [pine.fixtures.mysql :as fixtures]
            [pine.db.protocol :as protocol]
            )
  (:import pine.db.mysql.MysqlConnection)
  )

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:users "userId"
       :customers "customerId"
       }
      (protocol/references (MysqlConnection. nil) fixtures/schema "caseFiles")
      ))))


(deftest relation:test-schema-owns
  (testing "Get the references of a table"
    (is
     (=
      "caseFileId"
      (db/relation fixtures/schema :caseFiles :owns :documents))
     )))

(deftest relation:test-schema-owned-by
  (testing "Get the references of a table"
    (is
     (=
      "caseFileId"
      (db/relation fixtures/schema :documents :owned-by :caseFiles))
     )))
