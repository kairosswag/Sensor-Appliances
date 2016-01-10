package dryr.basestation.database;

import java.sql.*;
import java.util.LinkedList;
import java.util.List;

import dryr.common.json.beans.*;

public class DatabaseHelper {

	private static final String JDBC_DRIVER = "com.mysql.jdbc.Driver";
	private static final String DB_URL = "jdbc:mysql://localhost:3306/test?autoReconnect=true&amp;useSSL=false";

	private Connection conn = null;
	private Statement stmt = null;

	// Database credentials
	private static final String USER = "tomcatusr";
	private static final String PASS = "tomcatusrpw";

	public DatabaseHelper() {
		try {
			// This will load the MySQL driver, each DB has its own driver
			Class.forName(JDBC_DRIVER);
			// Setup the connection with the DB
			conn = DriverManager.getConnection(DB_URL, USER, PASS);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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
				ResultSet results = stmt.executeQuery("select * from test.devices" + stat);
				if (results != null) {
					while(results.next()) {
						BluetoothDevice device = new BluetoothDevice();
						device.setMac(results.getLong(1));
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
	
	
	
	public List<SensorDataPoint> getData(int amount) {
		return getData(amount, -1);
	}
	
	public List<SensorDataPoint> getData(int amount, long deviceId) {
		if (amount > 10000) return null; //sanity check
		
		List<SensorDataPoint> resultList = new LinkedList<SensorDataPoint>();
		if (conn != null) {
			try {
				stmt = conn.createStatement();
				String device = (deviceId < 0) ? "" : " where device=" + deviceId;
				ResultSet results = stmt.executeQuery("select * from test.data" + device + " order by date limit " + amount);
				if (results != null) {
					while(results.next()) {
						SensorDataPoint datum = new SensorDataPoint();
						datum.setTime(results.getLong(1));
						datum.setHumidity(results.getInt(2));
						resultList.add(datum);
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
	
	public void addPhone(long ip) {
		//insert statement
	}

}
