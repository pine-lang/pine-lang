(ns pine.db.mysql-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.mysql :as mysql]
            [pine.fixtures.mysql :as fixtures]
            [pine.db.connection :as connection]
            ))

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:users [["userId" nil] ["createdByUserId" nil]]
       :customers [["customerId" nil]]
       }
      (connection/references (pine.db.mysql.Mysql. "dummy" nil) fixtures/schema "caseFiles")
      ))))
