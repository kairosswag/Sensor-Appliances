package dryr.basestation.database;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement;
import java.util.LinkedList;
import java.util.List;

import dryr.basestation.type.MinMax;
import dryr.common.json.beans.HumiditySensorDataPoint;

public class DataPointDB {

	private Connection conn = null;
	private Statement stmt = null;
	
	public DataPointDB() {
		conn = (new DatabaseHelper()).getConnection();
	}
	
	public List<HumiditySensorDataPoint> getData(int amount) {
		return getData(amount, null);
	}
	
	public List<HumiditySensorDataPoint> getData(int amount, String mac) {
		if (amount > 10000) return null; //sanity check
		
		List<HumiditySensorDataPoint> resultList = new LinkedList<HumiditySensorDataPoint>();
		if (conn != null) {
			try {
				stmt = conn.createStatement();
				String device = (mac == null) ? "" : " where mac=" + mac;
				String limit = (amount < 0) ? "" : " limit " + amount;
				String query = "select * from Humidity" + device + " order by sample_time desc" + limit;
				ResultSet results = stmt.executeQuery(query);
				if (results != null) {
					while(results.next()) {
						HumiditySensorDataPoint datum = new HumiditySensorDataPoint();
						datum.setDate(results.getString(1));
						datum.setSensor(results.getString(2));
						datum.setHumidity(results.getFloat(3));
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

	public MinMax getMinMax(String mac, int seconds) {
		String queryString =
		"SELECT MIN(value), MAX(value) FROM Humidity\n" +
		"WHERE mac=?\n" +
		"AND (sample_time BETWEEN DATE_SUB(NOW(), INTERVAL ? SECOND) AND NOW())";
		PreparedStatement stmt;
		try {
			stmt = conn.prepareCall(queryString);
			stmt.setString(1, mac);
			stmt.setInt(2, seconds);
			ResultSet result = stmt.executeQuery();
			if (result.next()) {
				return new MinMax(result.getFloat(1), result.getFloat(2));
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}

		return null;
	}

	public void deleteData(String mac) {
		String queryString =
		"DELETE FROM Humidity WHERE mac=?;";
		try {
			PreparedStatement stmt = conn.prepareCall(queryString);
			stmt.setString(1, mac);
			stmt.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	public void deleteOldData(int hours) {
		String queryString =
		"DELETE FROM Humidity\n" +
		"WHERE sample_time <= DATE_SUB(NOW(), INTERVAL ? HOUR);";
		try {
			PreparedStatement stmt = conn.prepareCall(queryString);
			stmt.setInt(1, hours);
			stmt.executeUpdate();
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
}
