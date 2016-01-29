package dryr.common.json.beans;

public class Dry {
	private String mac;
	private boolean dry;
	
	public Dry() {
	}

	public Dry(String mac, boolean dry) {
		this.mac = mac;
		this.dry = dry;
	}

	public String getMac() {
		return mac;
	}

	public boolean getDry() {
		return dry;
	}
}
