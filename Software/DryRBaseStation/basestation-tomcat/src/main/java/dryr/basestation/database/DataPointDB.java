package dryr.basestation.database;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;

import dryr.common.json.beans.HumiditySensorDataPoint;

public class DataPointDB {

	private Connection conn = null;
	private Statement stmt = null;
	
	public DataPointDB() {
		conn = (new DatabaseHelper()).getConnection();
	}
	
	public List<HumiditySensorDataPoint> getData(int amount) {
		return getData(amount, -1);
	}
	
	public List<HumiditySensorDataPoint> getData(int amount, long deviceId) {
		if (amount > 10000) return null; //sanity check
		
		List<HumiditySensorDataPoint> resultList = new LinkedList<HumiditySensorDataPoint>();
		if (conn != null) {
			try {
				stmt = conn.createStatement();
				String device = (deviceId < 0) ? "" : " where Device=" + deviceId;
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

}
