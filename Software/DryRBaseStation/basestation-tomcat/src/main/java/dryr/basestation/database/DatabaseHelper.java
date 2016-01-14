package dryr.basestation.database;

import java.sql.*;
import java.util.LinkedList;
import java.util.List;

import dryr.common.json.beans.*;

public class DatabaseHelper {

	private static final String JDBC_DRIVER = "com.mysql.jdbc.Driver";
	private static final String DB_URL = "jdbc:mysql://localhost:3306/dryr.base?autoReconnect=true&amp;useSSL=false";

	private Connection conn = null;

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
	
	public Connection getConnection() {
		return conn;
	}	
	
	public void addPhone(long ip) {
		//insert statement
	}

}
