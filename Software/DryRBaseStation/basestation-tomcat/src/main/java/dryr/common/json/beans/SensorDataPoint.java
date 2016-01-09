package dryr.common.json.beans;

/**
 * Resembles a single data point captured by the sensor.
 * 
 * This follows the JavaBean conventions.
 * 
 * @author kairo
 *
 */
public class SensorDataPoint {
	private String sensor;
	private long time;
	private long humidity;
	
	public SensorDataPoint() {
	}

	public String getSensor() {
		return sensor;
	}

	public void setSensor(String sensor) {
		this.sensor = sensor;
	}

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	public long getHumidity() {
		return humidity;
	}

	public void setHumidity(long humidity) {
		this.humidity = humidity;
	}
	
}
