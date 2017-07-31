SELECT 10*20 FROM DUAL;

SHOW DATABASES;

USE awsMySQLdb;

CREATE TABLE employee (
id INT AUTO_INCREMENT PRIMARY KEY,
NAME VARCHAR(20),
dept VARCHAR(10),
salary INT(10)
);

DESC employee;

SHOW TABLES;

INSERT INTO employee VALUES(100,'Thomas','Sales',5000);
INSERT INTO employee VALUES(200,'Jason','Technology',5500);
INSERT INTO employee VALUES(300,'Mayla','Technology',7000);
INSERT INTO employee VALUES(400,'Nisha','Marketing',9500);
INSERT INTO employee VALUES(500,'Randy','Technology',6000);

INSERT INTO employee(NAME,dept) VALUES('Ritu', 'Accounting');

SELECT * FROM employee;


