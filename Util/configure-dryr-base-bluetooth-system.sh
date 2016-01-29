sed -i '/^ExecStart/ s/$/ -E/' /etc/systemd/system/bluetooth.target.wants/bluetooth.service
cp dryr.base.bluetooth.conf /etc/dbus-1/system.d/
