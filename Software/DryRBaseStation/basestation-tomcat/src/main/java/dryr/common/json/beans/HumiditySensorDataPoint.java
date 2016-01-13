package dryr.common.json.beans;

/**
 * Resembles a single data point captured by the sensor.
 * 
 * This follows the JavaBean conventions.
 * 
 * @author kairo
 *
 */
public class HumiditySensorDataPoint {
	private String date;
	private String sensor;
	private float humidity;
	
	public HumiditySensorDataPoint() {
	}

	public String getSensor() {
		return sensor;
	}

	public void setSensor(String sensor) {
		this.sensor = sensor;
	}

	public float getHumidity() {
		return humidity;
	}

	public void setHumidity(float humidity) {
		this.humidity = humidity;
	}
	
}
