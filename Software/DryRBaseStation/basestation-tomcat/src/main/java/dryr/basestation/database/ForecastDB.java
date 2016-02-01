package dryr.basestation.database;

import java.sql.Connection;

public class ForecastDB {
	
	private final Connection con;
	
	public ForecastDB() {
		con = (new DatabaseHelper()).getConnection();
	}

}
