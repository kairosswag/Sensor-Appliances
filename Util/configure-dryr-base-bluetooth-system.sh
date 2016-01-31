sed -i '/^ExecStart/ s/$/ -E/' /etc/systemd/system/bluetooth.target.wants/bluetooth.service
sed -i '/^ExecStart/ s/$/ -E/' /etc/systemd/system/dbus-org.bluez.service
systemctl daemon-reload
groupadd dryr
