(ns pine.fixtures
  (:require  [clojure.test :as t]))

(def schema {
             :customers "CREATE TABLE `customers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`)"
             :users "CREATE TABLE `users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `fullName` longtext COLLATE utf8_unicode_ci NOT NULL,
  `realEmail` longtext COLLATE utf8_unicode_ci,
  PRIMARY KEY (`id`)"
             :caseFiles "CREATE TABLE `caseFiles` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` longtext COLLATE utf8_unicode_ci NOT NULL,
  `customerId` int(11) DEFAULT NULL,
  `createdByUserId` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `FK_1` FOREIGN KEY (`userId`) REFERENCES `users` (`id`) ON DELETE SET NULL,
  CONSTRAINT `FK_2` FOREIGN KEY (`createdByUserId`) REFERENCES `users` (`id`) ON DELETE SET NULL,
  CONSTRAINT `FK_3` FOREIGN KEY (`customerId`) REFERENCES `customers` (`id`))"
             :documents "CREATE TABLE `documents` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` longtext COLLATE utf8_unicode_ci NOT NULL,
  `caseFileId` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_A2B0728870B85BE3` (`caseFileId`),
  CONSTRAINT `FK_1` FOREIGN KEY (`caseFileId`) REFERENCES `caseFiles` (`id`))"
             :folders "CREATE TABLE `folders` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` longtext COLLATE utf8_unicode_ci NOT NULL,
  `parentId` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `IDX_FE37D30F10EE4CEE` (`parentId`),
  CONSTRAINT `FK_FE37D30F10EE4CEE` FOREIGN KEY (`parentId`) REFERENCES `folders` (`id`) ON DELETE CASCADE"
             }
  )
