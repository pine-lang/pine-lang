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
      {:users ["userId" "createdByUserId"]
       :customers ["customerId"]
       }
      (protocol/references (MysqlConnection. "dummy" nil) fixtures/schema "caseFiles")
      ))))
