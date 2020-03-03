CREATE DATABASE cosmos;
CREATE USER 'myuser'@'%' IDENTIFIED BY 'cosmos123';
GRANT ALL PRIVILEGES ON cosmos.* TO 'myuser'@'%';
