package dryr.basestation.util;

import org.freedesktop.dbus.DBusConnection;
import org.freedesktop.dbus.exceptions.DBusException;

import dryr.base.bluetooth;

public class DBusUtil {
	private static DBusUtil instance = null;

	private bluetooth bluetooth;

	public static DBusUtil getInstance() {
		if (instance == null) {
			try {
				instance = new DBusUtil();
			} catch (DBusException e) {
				instance = null;
			}
		}

		return instance;
	}

	private DBusUtil() throws DBusException {
		DBusConnection conn = DBusConnection.getConnection(DBusConnection.SYSTEM);
		bluetooth = (bluetooth) conn.getRemoteObject("dryr.base.bluetooth", "/", bluetooth.class);
	}

	public void connectDevice(String mac) {
		bluetooth.ConnectDevice(mac);
	}

	public void disconnectDevice(String mac) {
		bluetooth.DisconnectDevice(mac);
	}
}
