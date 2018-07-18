(ns pine.fixtures
  (:require  [clojure.test :as t]))

(def schema {
                    :customers "CREATE TABLE `customers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)"
                    :caseFiles "CREATE TABLE `caseFiles` (
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
  CONSTRAINT `FK_1` FOREIGN KEY (`caseFileId`) REFERENCES `caseFiles` (`id`))"}
  )
