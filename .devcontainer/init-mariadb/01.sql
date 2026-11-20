-- Runs once, when the database volume is created.
CREATE DATABASE IF NOT EXISTS `test`;
GRANT ALL ON `test`.* TO 'admin'@'%';
