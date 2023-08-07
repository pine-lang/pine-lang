(ns pine.hints-test
  (:require [pine.hints :as sut]
            [clojure.test :refer :all]
            [pine.db.connection :as connection]
            [pine.db.connection-factory :as cf]
            [pine.db.postgres :as postgres]
            [clojure.string :as s]))

(deftest test-abbreviate:a_b_c
  (testing "abbreviate"
    (is (= "abc" (sut/abbreviate "a_b_c")))))

(deftest test-candidates
  (testing "candidates"
    (is (= #{"a""ab" "abc"} (sut/candidates "a" ["a" "b" "c" "ab" "bc" "abc"])))))

(deftest test-schema-hint
  (testing "schema hint"
    (is (= #{"public"} (sut/schema-hint (cf/create :postgres) nil "pu")))))

;; (postgres/get-metadata (cf/create :postgres))
;; ;; (connection/get-metadata (cf/create :postgres))



;; (schema-hint (cf/create :postgres) nil "pu")
