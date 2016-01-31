INSERT INTO Device
VALUES (?, ?, ?)
ON DUPLICATE KEY UPDATE
status_id=?,
rssi=?;
