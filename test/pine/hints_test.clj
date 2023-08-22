(ns pine.hints-test
  (:require [pine.hints :as sut]
            [clojure.test :refer :all]
            [pine.db.connection :as connection]
            [pine.db.connection-factory :as cf]
            [pine.db.postgres :as postgres]
            [clojure.string :as s]))

(def md (let [connection (cf/create :postgres)]
          (connection/get-metadata connection)
          ))

;; (deftest test-abbreviate:a_b_c
;;   (testing "abbreviate"
;;     (is (= "abc" (sut/abbreviate "a_b_c")))))

(deftest test-candidates:filter
  (testing "candidates"
    (is (= #{"a""ab" "abc"} (sut/candidates "a" ["a" "b" "c" "ab" "bc" "abc"])))))

(deftest test-schema-hint
  (testing "schema hint"
    (is (= #{"public"} (sut/schema-hint md nil "pu")))))

(deftest test-table-hint:without-schema
  (testing "table hint without schema"
    (is (= #{"document" "organization" } (sut/table-hint md {} "o")))))

(deftest test-table-hint:with-schema
  (testing "table hint with schema"
    (is (= #{"user"} (sut/table-hint md {:schema "x"} "us")))))

(deftest test-table-hint:without-context
  (testing "table hint without context"
    (is (= #{"user" "document" "organization"} (sut/table-hint md {} "")))))

(deftest test-table-hint:with-context
  (testing "table hint with context"
    (is (= #{"user"} (sut/table-hint md {:context {:entity "document"}} "")))))
