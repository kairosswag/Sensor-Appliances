package dryr.basestation.type;

public class MinMax {
	private final float min;
	private final float max;
	
	public MinMax(float min, float max) {
		this.min = min;
		this.max = max;
	}
	
	public float getMin() {
		return min;
	}
	
	public float getMax() {
		return max;
	}
}
