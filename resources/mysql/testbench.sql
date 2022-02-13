CREATE DATABASE tmp;

CREATE TABLE `customers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` longtext COLLATE utf8_unicode_ci NOT NULL,
  PRIMARY KEY (`id`));

INSERT INTO customers SET name = 'Acme Inc.';

-- Now test pine:
--
-- customers
--
-- or update:
-- customers id='1' | set! name='Acme Inc. updated'


