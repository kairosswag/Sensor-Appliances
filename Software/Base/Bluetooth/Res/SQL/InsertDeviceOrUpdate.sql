INSERT INTO Device
VALUES (?, 0, ?)
ON DUPLICATE KEY UPDATE
status_id=0,
rssi=?;
