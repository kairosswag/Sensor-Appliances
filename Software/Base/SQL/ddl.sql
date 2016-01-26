CREATE TABLE Status (
  id INTEGER NOT NULL,
  name CHAR(32) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE Device (
  mac CHAR(17) NOT NULL,
  status_id INTEGER NOT NULL,
  PRIMARY KEY (mac),
  FOREIGN KEY (status_id) REFERENCES Status(id)
  ON UPDATE CASCADE
  ON DELETE RESTRICT
);

CREATE TABLE Humidity (
  sample_time DATETIME NOT NULL,
  mac CHAR(17) NOT NULL,
  value FLOAT(32) NOT NULL,
  PRIMARY KEY (sample_time, mac),
  FOREIGN KEY (mac) REFERENCES Device(mac)
  ON UPDATE CASCADE
  ON DELETE CASCADE
);

CREATE VIEW Group_Humidity AS
SELECT DATE_SUB(sample_time, INTERVAL SECOND(sample_time) SECOND) AS group_time, mac, AVG(value)
FROM `Humidity`
GROUP BY mac, group_time;
