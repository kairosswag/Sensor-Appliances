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

	private final Connection conn;
	
	public DataPointDB() {
		conn = (new DatabaseHelper()).getConnection();
	}

	public List<HumiditySensorDataPoint> getData(String mac) {
		String queryString =
		"SELECT sample_time, mac, value FROM Humidity\n" +
		"WHERE mac=?\n" +
		"ORDER BY sample_time DESC;";
		try {
			PreparedStatement stmt = conn.prepareCall(queryString);
			stmt.setString(1, mac);
			ResultSet result = stmt.executeQuery();
			List<HumiditySensorDataPoint> resultList = new LinkedList<HumiditySensorDataPoint>();
			while (result.next()) {
				HumiditySensorDataPoint datum = new HumiditySensorDataPoint();
				datum.setDate(result.getString(1));
				datum.setSensor(result.getString(2));
				datum.setHumidity(result.getFloat(3));
				resultList.add(datum);
			}
			return resultList;
		} catch (SQLException e) {
			e.printStackTrace();
		}

		return null;
	}

	public List<HumiditySensorDataPoint> getDataNewerThan(String mac, String date) {
		String queryString =
		"SELECT sample_time, mac, value FROM Humidity\n" +
		"WHERE mac=?\n" +
		"AND sample_time > ?\n" +
		"ORDER BY sample_time DESC;";
		try {
			PreparedStatement stmt = conn.prepareCall(queryString);
			stmt.setString(1, mac);
			stmt.setString(2, date);
			ResultSet result = stmt.executeQuery();
			List<HumiditySensorDataPoint> resultList = new LinkedList<HumiditySensorDataPoint>();
			while (result.next()) {
				HumiditySensorDataPoint datum = new HumiditySensorDataPoint();
				datum.setDate(result.getString(1));
				datum.setSensor(result.getString(2));
				datum.setHumidity(result.getFloat(3));
				resultList.add(datum);
			}
			return resultList;
		} catch (SQLException e) {
			e.printStackTrace();
		}

		return null;
	}
	
	public MinMax getMinMax(String mac, int seconds) {
		String queryString =
		"SELECT MIN(value), MAX(value) FROM Humidity\n" +
		"WHERE mac=?\n" +
		"AND sample_time >= DATE_SUB(NOW(), INTERVAL ? SECOND);";
		try {
			PreparedStatement stmt = conn.prepareCall(queryString);
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
	
	public Float getAverage(String mac, int seconds) {
		String queryString =
		"SELECT AVG(value) FROM Humidity\n" +
		"WHERE mac=?\n" +
		"AND sample_time >= DATE_SUB(NOW(), INTERVAL ? SECOND);";
		try {
			PreparedStatement stmt = conn.prepareCall(queryString);
			stmt.setString(1, mac);
			stmt.setInt(2, seconds);
			ResultSet result = stmt.executeQuery();
			if (result.next()) {
				return result.getFloat(1);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}

		return null;
	}

	public void deleteData(String mac) {
		String queryString =
		"DELETE FROM Humidity\n" +
		"WHERE mac=?;";
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
