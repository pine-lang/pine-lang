(ns pine.db.mysql-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            [pine.db.mysql :as mysql]
            [pine.fixtures.mysql :as fixtures]
            [pine.db.protocol :as protocol]
            ))

(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:users [["userId" nil] ["createdByUserId" nil]]
       :customers [["customerId" nil]]
       }
      (protocol/references (pine.db.mysql.Mysql. "dummy" nil) fixtures/schema "caseFiles")
      ))))
