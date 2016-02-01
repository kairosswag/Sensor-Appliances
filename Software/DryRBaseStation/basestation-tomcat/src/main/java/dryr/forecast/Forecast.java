package dryr.forecast;
/*
 * @author Peter Noras
 * @version	1.8
 */

import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import dryr.basestation.database.DataPointDB;
import dryr.common.json.beans.HumiditySensorDataPoint;
import dryr.common.json.beans.Prediction;

public class Forecast {

	private String sensors[] = new String[100];
	private Date[] sDate = new Date[100];
	private float sHumidity[] = new float[100];
	private Date[] currentDate = new Date[100];
	private float currentHumidity[] = new float[100];
	private Prediction pred = null;
	private String deviceMac = null;

	public Forecast(String mac) throws Exception {
		deviceMac = mac;
		readDataBase(deviceMac);
	}
	
	public Prediction getPrediction() {
		return pred;
	}
	
	public void updatePrediction() {
		try {
			readDataBase(deviceMac);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private synchronized void readDataBase(String mac) throws Exception {
		DataPointDB dbData = null;
		try {
			// Result set get the result of the SQL query
			dbData = new DataPointDB();
			List<HumiditySensorDataPoint> res = dbData.getData(mac);
			pred = writeResultSet(res);

			/**
			 * Prepared Statement skeleton // TODO PreparedStatements can use
			 * variables and are more efficient preparedStatement = connect
			 * .prepareStatement(
			 * "insert into  ??? values (default, ?, ?, ?, ? , ?, ?)"); //
			 * Parameters start with 1 preparedStatement.setString(1, "Date");
			 * preparedStatement.setString(2, "Sensor");
			 * preparedStatement.setString(3, "Humidity");
			 * preparedStatement.executeUpdate();
			 * 
			 * // TODO Set DatabaseName preparedStatement = connect
			 * .prepareStatement("SELECT date, sensor, humidity from DATABASE");
			 * resultSet = preparedStatement.executeQuery();
			 * writeResultSet(resultSet);
			 * 
			 * // TODO Set DatabaseName resultSet = statement .executeQuery(
			 * "select * from DATABASENAME"); writeMetaData(resultSet);
			 **/

		} catch (Exception e) {
			throw e;
		} finally {
			if (dbData != null)
				dbData.close();
		}

	}

	@SuppressWarnings("unused")
	private synchronized void writeMetaData(ResultSet resultSet) throws SQLException {
		// Now get some metadata from the database
		// Result set get the result of the SQL query

		System.out.println("The columns in the table are: ");

		System.out.println("Table: " + resultSet.getMetaData().getTableName(1));
		for (int i = 1; i <= resultSet.getMetaData().getColumnCount(); i++) {
			System.out.println("Column " + i + " " + resultSet.getMetaData().getColumnName(i));
		}
	}

	// writes Set and makes forecast
	private synchronized Prediction writeResultSet(List<HumiditySensorDataPoint> results) throws SQLException {
		Prediction prediction = new Prediction();
		int t = 0;
		long difDate = 0;
		long forecastTime[] = new long[100];

		// ResultSet is initially before the first data set
		for (HumiditySensorDataPoint result : results) {
			// It is possible to get the columns via name
			// also possible to get the columns via the column number
			// which starts at 1
			// e.g. resultSet.getSTring(2);
			DateFormat dateFormat = new SimpleDateFormat("Myyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
			Date date;
			try {
				date = dateFormat.parse(result.getDate());
			} catch (ParseException e) {
				System.err.println("Could not parse date. Error: " + e);
				continue;
			}
			String sensor = result.getSensor();
			float humidity = result.getHumidity();
			System.out.println("Date: " + date);
			System.out.println("Sensor: " + sensor);
			System.out.println("Humidity: " + humidity);

			// search for the Sensor
			for (int i = 0; i < (sensors.length - 1); i++) {
				if (sensors[i].equals(sensor)) {
					t = i;
					currentDate[t] = date;
					currentHumidity[t] = humidity;
				} else {
					if (sensors[i] == null) {
						sensors[i] = sensor;
						sDate[i] = date;
						sHumidity[i] = humidity;
						t = i;
					} else {
						t = 0;
					}

				}
			}

			// make forecast
			if (sDate[t].getTime() < currentDate[t].getTime()) {
				difDate = currentDate[t].getTime() - sDate[t].getTime();
			} else {
				difDate = sDate[t].getTime() - currentDate[t].getTime();
			}

			if (currentHumidity[t] >= 46) {
				forecastTime[t] = (currentDate[t].getTime() + ((long) (currentHumidity[t] - 46) * difDate));
			} else {
				prediction.setEstimate(-1);
				prediction.setVariance(100);
				return prediction;
			}
		}
		prediction.setEstimate(forecastTime[t]);
		return prediction;

	}

}
