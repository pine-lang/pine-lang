(ns pine.db-test
  (:require [clojure.test :refer :all]
            [pine.db :as db]
            ))

(def *test-schema* {:caseFiles "CREATE TABLE `caseFiles` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` longtext COLLATE utf8_unicode_ci NOT NULL,
  `customerId` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `FK_1` FOREIGN KEY (`userId`) REFERENCES `users` (`id`) ON DELETE SET NULL,
  CONSTRAINT `FK_3` FOREIGN KEY (`customerId`) REFERENCES `customers` (`id`))"
                    :documents "CREATE TABLE `documents` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` longtext COLLATE utf8_unicode_ci NOT NULL,
  `caseFileId` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_A2B0728870B85BE3` (`caseFileId`),
  CONSTRAINT `FK_1` FOREIGN KEY (`caseFileId`) REFERENCES `caseFiles` (`id`))"})



(deftest references:test-schema
  (testing "Get the references of a table"
    (is
     (=
      {:users "userId"
       :customers "customerId"
       }
      (db/references *test-schema* "caseFiles")
      ))))


(deftest relation:test-schema-owns
  (testing "Get the references of a table"
    (is
     (=
      "caseFileId"
      (db/relation *test-schema* :caseFiles :owns :documents))
     )))

(deftest relation:test-schema-owned-by
  (testing "Get the references of a table"
    (is
     (=
      "caseFileId"
      (db/relation *test-schema* :documents :owned-by :caseFiles))
     )))
