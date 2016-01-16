package dryr.basestation.database;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;

import dryr.common.json.beans.BluetoothDevice;

public class BluetoothDeviceDB {
	
	private Connection conn = null;
	private Statement stmt = null;
	
	public BluetoothDeviceDB () {
		conn = (new DatabaseHelper()).getConnection();
	}
	
	public List<BluetoothDevice> getBluetoothDeviceList() {
		return getBluetoothDeviceList(-1);
	}

	public List<BluetoothDevice> getBluetoothDeviceList(int status) {
		List<BluetoothDevice> resultList = new LinkedList<BluetoothDevice>();
		if (conn != null) {
			try {
				stmt = conn.createStatement();
				String stat = (status < 0) ? "" : " where status=" + status;
				ResultSet results = stmt.executeQuery("select * from Device" + stat);
				if (results != null) {
					while(results.next()) {
						BluetoothDevice device = new BluetoothDevice();
						device.setMac(results.getString(1));
						device.setStatus(results.getInt(2));
						resultList.add(device);
					}
				}
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} finally {
				try {
					conn.close();
				} catch (SQLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return resultList;
	}

	public BluetoothDevice getBluetoothDevice(String deviceMac) {
		if (conn != null) {
			try {
				stmt = conn.createStatement();
				String stat = " where mac='" + deviceMac + "'";
				ResultSet results = stmt.executeQuery("select * from Device" + stat);
				if (results != null) {
					while(results.next()) {
						BluetoothDevice device = new BluetoothDevice();
						device.setMac(results.getString(1));
						device.setStatus(results.getInt(2));
						return device;
					}
				}
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} finally {
				try {
					conn.close();
				} catch (SQLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		return null;
	}

}
