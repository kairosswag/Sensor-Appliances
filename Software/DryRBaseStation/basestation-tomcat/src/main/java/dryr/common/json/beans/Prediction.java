package dryr.common.json.beans;

public class Prediction {
	private long estimate;
	private float variance;
	
	public Prediction() {
	}

	public long getEstimate() {
		return estimate;
	}

	public void setEstimate(long estimate) {
		this.estimate = estimate;
	}

	public float getVariance() {
		return variance;
	}

	public void setVariance(float variance) {
		this.variance = variance;
	}

}
