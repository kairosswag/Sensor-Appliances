/*
 * @author Peter Noras
 * @version	1.8
 */

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Date;

public abstract class forecast {
	
	private Connection connect = null;
	private Statement statement = null;
	private PreparedStatement preparedStatement = null;
	private ResultSet resultSet = null;
	private String sensors[] = new String[100];
	private Date[] sDate = new Date[100];
	private double sHumidity[] = new double[100];
	private Date[] currentDate = new Date[100];
	private double currentHumidity[] = new double[100];
	
	public forecast() throws Exception {
		readDataBase();
	}
	
	public synchronized void readDataBase() throws Exception {
	    try {
	      // TODO This will load the MySQL driver, each DB has its own driver
	      Class.forName("com.mysql.jdbc.Driver");
	      // TODO Setup the connection with the DB
	      connect = DriverManager
	          .getConnection("jdbc:mysql://localhost/????"
	              + "user=sqluser&password=sqluserpw");

	      // Statements allow to issue SQL queries to the database
	      statement = connect.createStatement();
	      // Result set get the result of the SQL query
	      //TODO set DatabaseName
	      resultSet = statement
	          .executeQuery("select * from // TODO DatabaseName");
	      writeResultSet(resultSet);

	      // TODO PreparedStatements can use variables and are more efficient
	      preparedStatement = connect
	          .prepareStatement("insert into  ??? values (default, ?, ?, ?, ? , ?, ?)");
	      // Parameters start with 1
	      preparedStatement.setString(1, "Date");
	      preparedStatement.setString(2, "Sensor");
	      preparedStatement.setString(3, "Humidity");
	      preparedStatement.executeUpdate();

	      // TODO Set DatabaseName
	      preparedStatement = connect
	          .prepareStatement("SELECT date, sensor, humidity from DATABASE");
	      resultSet = preparedStatement.executeQuery();
	      writeResultSet(resultSet);

	      // TODO Set DatabaseName     
	      resultSet = statement
	      .executeQuery("select * from DATABASENAME");
	      writeMetaData(resultSet);
	      
	    } catch (Exception e) {
	      throw e;
	    } finally {
	      close();
	    }

	  }

	  private void writeMetaData(ResultSet resultSet) throws SQLException {
	    //   Now get some metadata from the database
	    // Result set get the result of the SQL query
	    
	    System.out.println("The columns in the table are: ");
	    
	    System.out.println("Table: " + resultSet.getMetaData().getTableName(1));
	    for  (int i = 1; i<= resultSet.getMetaData().getColumnCount(); i++){
	      System.out.println("Column " +i  + " "+ resultSet.getMetaData().getColumnName(i));
	    }
	  }

	  //writes Set and makes forecast
	  private long writeResultSet(ResultSet resultSet) throws SQLException {
		  int t=0;
	      Boolean isInArray=false;
	      long difDate = 0;
	      double difHumidity = 0;
	      long forecastTime[] = new long[100];
	      
	      // ResultSet is initially before the first data set
	      while (resultSet.next()) {
	    	  // It is possible to get the columns via name
	    	  //also possible to get the columns via the column number
	    	  //which starts at 1
	    	  //e.g. resultSet.getSTring(2);
	    	  Date date = resultSet.getDate("Date");
	    	  String sensor = resultSet.getString("Sensor");
	    	  Double humidity = resultSet.getDouble("Humidity");
	    	  System.out.println("Date: " + date);
	    	  System.out.println("Sensor: " + sensor);
	    	  System.out.println("Humidity: " + humidity);
	    	  
	    	  
	    	  //search for the Sensor
	    	  for (int i=0; i<(sensors.length-1);i++){
	    		  if(sensors[i].equals(sensor)){
	    			  t=i;
	    			  isInArray=true;
	    			  currentDate[t]= date;
	    			  currentHumidity[t]=humidity;
	    		  }
	    		  else{
	    			  if(sensors[i]==null){
	    				  sensors[i]=sensor;
	    				  sDate[i]=date;
	    				  sHumidity[i]=humidity;
	    				  t=i;
	    				  isInArray=true;
	    			  	}
	    			  else{
	    				  t=0;
	    				  isInArray=false;
	    			  }
	    			  
	    		  }
	    	  }
	    	  
	    	  //make forecast
	    	  if (sDate[t].getTime() < currentDate[t].getTime()){
	    		  difDate=currentDate[t].getTime() - sDate[t].getTime();
	    	  }
	    	  else{
	    		  difDate=sDate[t].getTime() - currentDate[t].getTime();
	    	  }
	    	  
	    	  difHumidity=sHumidity[t] - currentHumidity[t];
	    	  
	    	  if (currentHumidity[t] >= 46){
	    		  forecastTime[t]= (currentDate[t].getTime() + ((long) (currentHumidity[t]-46) * difDate));
	    	  }
	    	  else{
	    		  return 0;
	    	  }
	      }
	      return forecastTime[t];
	      
	  }
	  
	  // You need to close the resultSet
	  private void close() {
	    try {
	      if (resultSet != null) {
	        resultSet.close();
	      }

	      if (statement != null) {
	        statement.close();
	      }

	      if (connect != null) {
	        connect.close();
	      }
	    } catch (Exception e) {

	    }
	  }

}
