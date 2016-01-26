package dryr.common.json.beans;

public class HumidityTreshold {
	
	private final static float STANDARD_TRESHOLD = 50f; //the unbiased treshold to begin with (in % rel. hum.)
	//private User user; //for future use
	private float userBias; 
	private float treshold; //the direct treshold the user set

	public HumidityTreshold() {
		
	}

	/**
	 * Returns the standard Treshold. This should not
	 * @return
	 */
	public float getStandardTreshold() {
		return STANDARD_TRESHOLD;
	}

	public float getUserBias() {
		return userBias;
	}

	public void setUserBias(float userBias) {
		this.userBias = userBias;
		this.setTreshold(STANDARD_TRESHOLD + userBias);
	}

	public float getTreshold() {
		return treshold;
	}

	public void setTreshold(float treshold) {
		this.treshold = treshold;
		this.userBias = treshold - STANDARD_TRESHOLD;
	}
	
	


}
